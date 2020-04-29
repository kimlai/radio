const fs = require("fs");
const https = require("https");
const R = require("ramda");

/*
 * Prints out dead tracks.
 */
const contents = fs.readFileSync("./tracks", "utf8");

const splitByEmptyLine = string => string.split("\n\n");

const splitByLine = string => string.split("\n");

const fields = ["artist", "title", "source", "cover"];

const customFormatToObject = lines =>
  lines.reduce((acc, line, i) => {
    if (fields[i] === "cover") {
      return R.assoc(fields[i], "/public/covers/" + line, acc);
    }
    return R.assoc(fields[i], line, acc);
  }, {});

const addStreamingInfo = track => {
  if (R.contains("youtube", track.source)) {
    return R.assoc(
      "youtube",
      { id: new URLSearchParams(new URL(track.source).search).get("v") },
      track
    );
  }
  if (R.contains("soundcloud", track.source)) {
    return R.pipe(
      R.assoc("source", track.source.split(" ")[0]),
      R.assoc("soundcloud", { stream_url: track.source.split(" ")[1] })
    )(track);
  }
  throw new Error("Source non reconnue");
};

const isAlive = track => {
  const req = https.get(
    track.soundcloud.stream_url + "?client_id=cc1bf58dc4b00ed757ab2f76deaa3d70",
    resp => {
      if (resp.statusCode === 404) {
        console.log(track.source);
      }
    }
  );
};

const res = R.pipe(
  splitByEmptyLine,
  R.map(splitByLine),
  R.map(R.reject(R.equals(""))),
  R.map(customFormatToObject),
  R.map(addStreamingInfo),
  R.filter(R.has("soundcloud")),
  R.map(isAlive)
)(contents);
