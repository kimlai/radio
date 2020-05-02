const fs = require("fs");
const sharp = require("sharp");

fs.readdirSync("./covers").forEach(file => {
  sharp("./covers/" + file)
    .resize(400, 400)
    .toFile("./static/covers/" + file, (err, info) => {
      if (err) {
        throw new Error(err);
      }
    });
});
