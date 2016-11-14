import React from "react";
import {Socket} from "phoenix"

class Row extends React.Component {
  render() {
    return (
      <div className="row">
        {this.props.children}
      </div>
    );
  }
}

class PotentialRooms extends React.Component {
  render() {
    if (this.props.rooms.length > 0) {
      return (
        <Row>
          <div className="col-lg-12">
            <p>Other possibilities</p>
            <ul>
              {this.props.rooms.map((room, i) => {
                return <li key={i}>{room}</li>
              })}
            </ul>
          </div>
        </Row>
      );
    } else {
      return null;
    }
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
    return <input onClick={() => this.props.onClick(this.props.activeValue)} className={classNames} type="submit" value={this.props.title} />;
  }
}

class Room extends React.Component {
  render() {
    return (
      <Row>
        <div className="col-lg-12">
          <h1>{this.props.name}</h1>
        </div>
      </Row>
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
        <Room name={this.state.room} />
        <PotentialRooms rooms={this.state.potentialRooms} />
        <ModeButton onClick={this.updateMode} activeState={this.state.mode} activeValue="manual" title="Manual" />
        <ModeButton onClick={this.updateMode} activeState={this.state.mode} activeValue="auto" title="Auto" />
      </div>
    );
  }
}
