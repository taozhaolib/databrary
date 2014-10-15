'use strict';

app.service('exportService', [
  'constantService',
  function (constants) {

    var seconds = new Date().getTime() / 1000; //for timing the export

    var dataExport = {};


    dataExport.downloadCSV = function(volume){

      volume.get(['records', 'containers']).then(createCSV);

    };

    function createCSV(volume){

      var input = volume;

      var payload = '';

      var containers = input.containers;
      var records = input.records;

      var baseHeaders = [ //these are static, tied to the volume
        'session id',
        'session name',
        'session date'
      ];


      /*helper object and arrays*/
      var catCounts = getCategoryCounts(containers, records);
      var headerRef = sortHeaderIdx(makeHeaderIndex(records, constants.metric)); //sorted array version of headerIdx
      var colCoords = makeHeaderCoords(headerRef, catCounts); //object array that represents the headers as category,metric coords in order - [{cat:metric},...]


      var moreHeaders = makeHeadersText(headerRef, catCounts); //turn header index into column names

      var header = baseHeaders.concat(moreHeaders).join(',');


      /*create CSV body data*/
      var body = createCSVBody(containers, records, colCoords);


      payload = header + '\n' + body;

      createPayload(payload, volume);

    }

    function escapeCSV(input) {
      if (input === undefined)
        return '';
      input = input.toString();
      if (input.contains('"') || input.contains(',') || input.contains('\n'))
        input = '"'+input.replace(/"/g, '""')+'"';
      return input;
    }

    function byNumber(a,b) {
      return a-b;
    }

    function createCSVBody(containers, records, headerReference){

      var body = '';

      for(var k in containers){
        if(!containers[k].top){ //exclude materials for now

          var ssRow = [];

          ssRow.push(containers[k].id);
          ssRow.push(containers[k].name);
          ssRow.push(containers[k].date);
          var recIdArr = [];
          for (var j in containers[k].records){ //get an array of the record IDs for each container in advance

            recIdArr.push(containers[k].records[j].id);
          }


          var idx = 0; //create and index for the record loop so we have more control over the loop logic
          for(var l = 0; l < headerReference.length; l++ ){

            var cellCat = headerReference[l].category;
            var cellMet = headerReference[l].metric.toString();


            if (idx < recIdArr.length){

              var recID = recIdArr[idx].toString();
              var recMetrics = Object.keys(records[recID].measures).sort(byNumber);

              if(records[recID].category !== cellCat){

                ssRow.push('');

              } else {

                ssRow.push(records[recID].measures[cellMet]);

                if(recMetrics.length <= 1 || cellMet === recMetrics[recMetrics.length-1]){
                  idx++; //only advance if we are done with all the metrics in this record
                }
              }

            }

          }

          body += ssRow.map(escapeCSV).join(',') + '\n';
        }
      }
      return body;
    }


    function createPayload(payload, volume){

      var filename = volume.id + "-" + volume.name.replace(/[\0-,/?\\]+/g, '_') + '.csv';
      var uri = 'data:text/csv;charset=utf-8,' + encodeURI(payload);

      if(window.navigator.msSaveOrOpenBlob){
        var data = [payload];
        var blobObject = new Blob(data);
        window.navigator.msSaveOrOpenBlob(blobObject, filename);

      } else {
        var link = document.createElement('a');
        link.href = uri;

        //link.style = "visibility:hidden"; //check this on safari, throws assign on readonly error
        link.download = filename;

        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);

      }



      var timenow = new Date().getTime() / 1000;
      var timeto = (timenow - seconds);

      console.log("Export process took: " + timeto + " seconds");
    }

    /*--------------functions for creating helper objects and arrays above-----------------*/

    function makeHeaderCoords(headerIndexArr, categoryCountsObj){

      var output = [];

      headerIndexArr.forEach(function(item){

        var catID = constants.category[item.category].id;

        for(var z = 0; z < categoryCountsObj[catID]; z++){
          if(item.metrics.length < 1){ //this is not future proof, may want to visit the data model.
            output.push({"category": catID, "metric": ''});
          }

          for(var m = 0; m < item.metrics.length; m++){
            var metID = constants.metric[item.metrics[m]].id;

            output.push({"category": catID, "metric": metID});
          }
        }


      });


      return output;


    }

    function makeHeadersText(headerIndexArr, categoryCountsObj){

      var output = [];

      headerIndexArr.forEach(function(item){

        var cat = constants.category[item.category];
        var catID = cat.id;
        var catText = cat.name;

        for(var z = 0; z < categoryCountsObj[catID]; z++){
          if(item.metrics.length < 1){ //this is not future proof, may want to visit the data model.
            output.push(catText);
          }

          for(var m = 0; m < item.metrics.length; m++){
            var met = constants.metric[item.metrics[m]];
            var metText = met.display || met.name;

            if(z>0){
              output.push(catText + ' ' + (z + 1).toString() + ' ' + metText);
            } else {
              output.push(catText + ' ' + metText);
            }

          }
        }

      });


      return output;

    }


    function makeHeaderIndex(recObj, metrics){

      var tableObj = {};

      for(var key in recObj){

        var cat = recObj[key].category;

        if (!(cat in tableObj)){
          tableObj[cat] = {};
        }

        for(var i in recObj[key].measures){

          tableObj[cat][i] = metrics[i].name;

        }

      }
      return tableObj;


    }

    function sortHeaderIdx(headerIdx){

      var newIdx = [];

      var catKeysSorted = Object.keys(headerIdx).sort(byNumber);

      for(var v = 0; v < catKeysSorted.length; v++){

        var metricsArrSorted = Object.keys(headerIdx[catKeysSorted[v]]).sort(byNumber);

        newIdx.push({"category": catKeysSorted[v], "metrics": metricsArrSorted});
      }


      return newIdx;

    }

    function getCategoryCounts(contObj, recObj){
      /*count the categories per record and post the largest for each*/
      var maxCatCounts = {};

      for(var i in contObj){

        var container = contObj[i].id;

        if(!(container in maxCatCounts)){

          maxCatCounts[container] = {};

        }

        var recArr = [];
        for(var j in contObj[i].records){

          recArr.push(contObj[i].records[j].id);

        }

        if(recArr.length > 0){
          for(var k=0; k<recArr.length; k++){
            var category = recObj[recArr[k]].category;
            if( maxCatCounts[container][category] > 0){
              maxCatCounts[container][category] += 1;
            } else {
              maxCatCounts[container][category] = 1;
            }
          }
        }
      }

      /*for maxCatCounts, get all categories into an array for each category */
      var topCats = {};

      for (var l in maxCatCounts){
        for(var m in maxCatCounts[l]){
          var cat = m;
          if(!(cat in topCats)){
            topCats[m] = [];
          }

          topCats[m].push(maxCatCounts[l][m]);

        }

      }

      /* get the max # of iterations for each category*/
      for(var n in topCats){
        topCats[n] = Math.max.apply(null, topCats[n]);
      }

      return topCats;

    }

    return dataExport;
  }

]);
