'use strict';

function main(setNum, probNum) {
  const setMod,
        probFunc;

  try {
    setMod   = require('./set_' + setNum);
  } catch (e) {
    process.stderr.write('ERROR:\n  ' + e.message);
    process.exit(1);
  }

  probFunc = 'problem' + probNum;

  if (setMod[probFunc] == undefined) {
    process.stderr.write('ERROR:\n  Cannot find problem '
        + probNum + ' in set ' + setNum);
    process.exit(1);
  }

  var timeStart = process.hrtime();
  var solution  = setMod[probFunc]();
  var timeTup   = process.hrtime(timeStart);

  // convert time to μs
  var timeElapsed = (timeTup[0] * 1000) + (timeTup[1] / 1000);

  // round to integer value
  Math.round(timeElapsed);

  process.stdout.write(solution + '\n' + Math.round(timeElapsed));
}

main.apply(null, process.argv.slice(2, 4));

