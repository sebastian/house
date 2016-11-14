import "phoenix_html"

import React from "react";
import ReactDOM from "react-dom";

import $ from "jquery";
import _ from "lodash";

import {PageRoot} from "./root";

if (window.pageConfig !== undefined) {
  window.pageConfig.onLoad.call(this, (params) => {
    const element = document.getElementById("page-frame");
    ReactDOM.render(<PageRoot {...params} />, element);
  });
}
