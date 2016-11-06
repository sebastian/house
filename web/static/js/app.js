import "phoenix_html"

import React from "react";
import ReactDOM from "react-dom";

import $ from "jquery";
import _ from "lodash";

import {PageRoot} from "./root";

const element = document.getElementById("page-frame");
ReactDOM.render(<PageRoot />, element);
