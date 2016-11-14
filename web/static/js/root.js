import React from "react";
import {Socket} from "phoenix"

class Panel extends React.Component {
  render() {
    return (
      <div className="panel panel-default">
        <div className="panel-heading">
          <h1 className="panel-title">
            {this.props.title}
          </h1>
        </div>
        <div className="panel-body">
          {this.props.children}
        </div>
      </div>
    );
  }
}

class ModeButton extends React.Component {
  render() {
    let classNames = "btn btn-lg ";
    if (this.props.activeState === this.props.activeValue) {
      classNames = classNames + "btn-success";
    } else {
      classNames = classNames + "btn-default";
    }
    return (
      <button onClick={() => this.props.onClick(this.props.activeValue)} className={classNames} type="submit">
        {this.props.title}
      </button>
    );
  }
}

class Rooms extends React.Component {
  render() {
    let otherRooms = null;
    if (this.props.otherRooms.length > 0) {
      otherRooms = (
        <div>
          <p>Other possibilities</p>
          <ul>
            {this.props.rooms.map((room, i) => {
              return <li key={i}>{room}</li>
            })}
          </ul>
        </div>
      );
    }
    return (
      <Panel title="Room">
        <p>
          You are currently in the <strong>{this.props.room}</strong>
        </p>

        {otherRooms}
      </Panel>
    );
  }
}

export class PageRoot extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      room: props.room,
      potentialRooms: props.potentialRooms,
      mode: props.mode,
    };

    const socket = new Socket("/socket", {});
    socket.connect();
    let channel = socket.channel("updates:all", {})
    channel.join()
      .receive("ok", resp => { console.log("Joined successfully", resp) })
      .receive("error", resp => { console.log("Unable to join", resp) })

    this.serverSentRoomUpdate = this.serverSentRoomUpdate.bind(this);
    this.serverSentModeUpdate = this.serverSentModeUpdate.bind(this);
    this.updateMode = this.updateMode.bind(this);
    channel.on("new_room", this.serverSentRoomUpdate);
    channel.on("new_mode", this.serverSentModeUpdate);

    this.channel = channel;
  }

  serverSentRoomUpdate(event) {
    this.setState({room: event.name, potentialRooms: event.potential_rooms});
  }

  serverSentModeUpdate(event) {
    this.setState({mode: event.mode});
  }

  updateMode(mode) {
    this.setState({mode});
    this.channel.push("set_mode", mode);
  }

  render() {
    return (
      <div>
        <Rooms room={this.state.room} otherRooms={this.state.potentialRooms} />
        <Panel title="Mode">
          <ModeButton className="col-12-md" onClick={this.updateMode} activeState={this.state.mode} activeValue="manual" title="Manual" />
          <ModeButton onClick={this.updateMode} activeState={this.state.mode} activeValue="auto" title="Auto" />
        </Panel>
      </div>
    );
  }
}
