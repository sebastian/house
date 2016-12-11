import React from "react";
import {Socket} from "phoenix"

class NakedPanel extends React.Component {
  render() {
    return (
      <div className="panel panel-default">
        <div className="panel-heading">
          <h1 className="panel-title">
            {this.props.title}
          </h1>
        </div>
        {this.props.children}
      </div>
    );
  }
}

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
            {this.props.otherRooms.map((room, i) => {
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
      roomSensors: props.roomSensors,
    };

    const socket = new Socket("/socket", {});
    socket.connect();
    let channel = socket.channel("updates:all", {})
    channel.join()
      .receive("ok", resp => { console.log("Joined successfully", resp) })
      .receive("error", resp => { console.log("Unable to join", resp) })

    this.serverSentRoomUpdate = this.serverSentRoomUpdate.bind(this);
    this.serverSentModeUpdate = this.serverSentModeUpdate.bind(this);
    this.serverSentRoomSensorUpdate = this.serverSentRoomSensorUpdate.bind(this);
    this.updateMode = this.updateMode.bind(this);
    channel.on("new_room", this.serverSentRoomUpdate);
    channel.on("new_mode", this.serverSentModeUpdate);
    channel.on("sensor_data_update", this.serverSentRoomSensorUpdate);

    this.channel = channel;
  }

  serverSentRoomUpdate(event) {
    this.setState({room: event.name, potentialRooms: event.potential_rooms});
  }

  serverSentModeUpdate(event) {
    this.setState({mode: event.mode});
  }

  serverSentRoomSensorUpdate(event) {
    this.setState({roomSensors: event.room_sensors});
  }

  updateMode(mode) {
    this.setState({mode});
    this.channel.push("set_mode", mode);
  }

  render() {
    return (
      <div className="container">
        <div className="row">
          <div className="col-xs-12 col-sm-6 col-md-4">
            <Rooms room={this.state.room} otherRooms={this.state.potentialRooms} />
          </div>
          <div className="col-xs-12 col-sm-6 col-md-4">
            <Panel title="Mode">
              <ModeButton onClick={this.updateMode} activeState={this.state.mode} activeValue="auto" title="Auto" />
              {' '}
              <ModeButton className="col-12-md" onClick={this.updateMode} activeState={this.state.mode} activeValue="manual" title="Manual" />
              {' '}
              <ModeButton onClick={this.updateMode} activeState={this.state.mode} activeValue="away" title="Away" />
              {' '}
              <ModeButton onClick={this.updateMode} activeState={this.state.mode} activeValue="presence_only" title="Presence only" />
            </Panel>
          </div>
          <div className="col-xs-12 col-sm-12 col-md-4">
            <NakedPanel title="State">
              <table className="table">
                <thead>
                  <tr>
                    <th>Name</th>
                    <th>Lux</th>
                    <th>Temperature</th>
                  </tr>
                </thead>
                <tbody>
                  {this.state.roomSensors.map((data, i) => {
                    return (
                      <tr key={i}>
                        <td>{data.name}</td>
                        <td>{data.lux}</td>
                        <td>{data.temperature} &#8451;</td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </NakedPanel>
          </div>
        </div>
      </div>
    );
  }
}
