const fs = require("fs");
const path = require("path");
const R = require("ramda");

/*
 * Read the "tracks" file and compile it to paginated json data
 * The build assumes that tracks are separated by empty lines,
 * and that they're formatted like this
 * ------
 * artist
 * title
 * source
 * cover
 * ------
 */
const NUMBER_OF_PLAYLISTS = 30;
const PAGE_SIZE = 20;

const contents = fs.readFileSync("./tracks", "utf8");

const splitByEmptyLine = string => string.split("\n\n");

const splitByLine = string => string.split("\n");

const fields = ["artist", "title", "source", "cover"];

const customFormatToObject = lines =>
  lines.reduce((acc, line, i) => {
    if (fields[i] === "cover") {
      return R.assoc(fields[i], "/covers/" + line, acc);
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

const paginate = tracks => {
  const pages = R.splitEvery(PAGE_SIZE, tracks);
  return pages.map((pageTracks, i) => ({
    tracks: pageTracks,
    next_href:
      pages.length === i + 1 ? null : "/json/tracks/page_" + (i + 2) + ".json"
  }));
};

const writeJSONFile = (file, contents) => {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  fs.writeFileSync(file, JSON.stringify(contents));
};

const shuffle = array => {
  const a = array.slice();
  let j, x, i;
  for (i = a.length - 1; i > 0; i--) {
    j = Math.floor(Math.random() * (i + 1));
    x = a[i];
    a[i] = a[j];
    a[j] = x;
  }
  return a;
};

const buildPlaylists = tracks => {
  const playlists = [];
  for (let i = 0; i < NUMBER_OF_PLAYLISTS; i++) {
    playlists.push(shuffle(tracks));
  }

  return playlists;
};

const writeJSONFiles = dest => files => {
  files.map((file, i) => writeJSONFile(dest + (i + 1) + ".json", file));
};

const res = R.pipe(
  splitByEmptyLine,
  R.map(splitByLine),
  R.map(R.reject(R.equals(""))),
  R.map(customFormatToObject),
  R.map(addStreamingInfo),
  tracks => tracks.map((t, i) => R.assoc("id", i, t)),
  R.tap(
    R.pipe(
      buildPlaylists,
      R.map(paginate),
      playlists =>
        playlists.map((playlist, i) =>
          writeJSONFiles("static/json/playlists/" + i + "/page_")(playlist)
        )
    )
  ),
  paginate,
  writeJSONFiles("static/json/tracks/page_")
)(contents);
