import Center from "../layout-components/Center.js";
import Cluster from "../layout-components/Cluster.js";
import Cover from "../layout-components/Cover.js";
import Imposter from "../layout-components/Imposter.js";
import Sidebar from "../layout-components/Sidebar.js";
import Stack from "../layout-components/Stack.js";
import initializePlayer from "./initializePlayer";
import { Elm } from "../elm/src/Main.elm";

var tag = document.createElement("script");
tag.src = "https://www.youtube.com/iframe_api";
var firstScriptTag = document.getElementsByTagName("script")[0];
firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

var app = Elm.Main.init({
  node: document.getElementById("radio"),
  flags: Math.floor(Math.random() * 29)
});

window.onYouTubeIframeAPIReady = function() {
  initializePlayer("cc1bf58dc4b00ed757ab2f76deaa3d70", app);
};
