const fs = require("fs");
const R = require("ramda");
const https = require("https");

/*
 * Ugly one-off script to import the api of the previous version of the radio
 */

const getJSON = url => {
  return new Promise((resolve, reject) => {
    const req = https
      .get(url, resp => {
        let data = "";

        resp.on("data", chunk => {
          data += chunk;
        });

        resp.on("end", () => resolve(JSON.parse(data)));
      })
      .on("error", err => reject(err));
  });
};

const fetchTracks = url => {
  return getJSON(url).then(res => {
    if (res.next_href === null) {
      return res.tracks;
    }
    return fetchTracks("https://radio.kimlaitrinh.me" + res.next_href).then(
      tracks => res.tracks.concat(tracks)
    );
  });
};

const sanitize = string =>
  string
    .toLowerCase()
    .replace(" ", "_")
    .replace(/[^a-zA-Z0-9_]/g, "");

const downloadCover = track => {
  if (!track.cover) {
    return R.assoc("cover", "no_cover", track);
  }
  const filename = sanitize(track.artist + "_" + track.title) + ".jpg";
  const dest = "public/covers/" + filename;
  const file = fs.createWriteStream("public/covers/" + filename);
  const request = https
    .get(track.cover.replace("-large.jpg", "-t500x500.jpg"), function(
      response
    ) {
      response.pipe(file);
    })
    .on("error", function(err) {
      console.log(track.cover);
    });

  return R.assoc("cover", filename, track);
};

const source = track =>
  track.soundcloud
    ? track.source + " " + track.soundcloud.stream_url
    : track.source;

const writeToFile = track => {
  fs.appendFileSync(
    "tracks",
    [track.artist, track.title, source(track), track.cover].join("\n") + "\n\n"
  );
};

fetchTracks("https://radio.kimlaitrinh.me/api/latest-tracks").then(
  R.pipe(
    R.map(downloadCover),
    R.map(writeToFile)
  )
);
