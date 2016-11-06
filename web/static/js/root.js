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
      room: "Unknown room",
      potentialRooms: [],
    };

    const socket = new Socket("/socket", {});
    socket.connect();
    let channel = socket.channel("room:presence", {})
    channel.join()
      .receive("ok", resp => { console.log("Joined successfully", resp) })
      .receive("error", resp => { console.log("Unable to join", resp) })

    this.updateRoom = this.updateRoom.bind(this);
    channel.on("new_room", this.updateRoom);
  }

  updateRoom(event) {
    this.setState({room: event.name, potentialRooms: event.potential_rooms});
  }

  render() {
    return (
      <div>
        <Room name={this.state.room} />
        <PotentialRooms rooms={this.state.potentialRooms} />
      </div>
    );
  }
}
