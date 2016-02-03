'use strict';

/************************************************************************************
 *                                     - main -                                     *
 *                                                                                  *
 * Function responsible for fetching the problem set module specified by 'setNum',  *
 * calling its problem function specified by 'probNum', printing the results to     *
 * stdout, and handling all possible errors encountered along the way.              *
/************************************************************************************/
function main(setNum, probNum) {
  var setMod,       // module housing requested problem function
      probFunc,     // zero-arity function which returns solution
      timeStartTup, // tuple [ms, ns] from abritrary time ref
      timeDiffTup,  // tuple [ms, ns] time elapsed while solving problem
      solution,     // solution to requested problem
      timeElapsed;  // time elapsed (μs) rounded to the nearest integer

  // ensure request problem set exists
  try {
    setMod = require('./set_' + setNum);
  } catch (e) {
    process.stderr.write('ERROR:\n  ' + e.message);
    process.exit(1);
  }

  probFunc = 'problem' + probNum;

  // ensure requested problem function exists in requested problem set
  if (setMod[probFunc] == undefined) {
    process.stderr.write('ERROR:\n  Cannot find problem '
        + probNum + ' in set ' + setNum);
    process.exit(1);
  }

  timeStartTup = process.hrtime();             // start timer
  solution     = setMod[probFunc]();           // call problem function
  timeDiffTup  = process.hrtime(timeStartTup); // take diff from start time

  // combine [ms, ns] time tuple, converting to μs
  timeElapsed = (timeDiffTup[0] * 1000) + (timeDiffTup[1] / 1000);

  // round to nearest integer
  Math.round(timeElapsed);

  // write solution to stdout, delimiting solution - time elapsed with a newline
  process.stdout.write(solution + '\n' + Math.round(timeElapsed));
}

// call main function with first two unique cmd line args: 'setNum' and 'probNum'
main.apply(null, process.argv.slice(2, 4));
