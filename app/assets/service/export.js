'use strict';

app.service('exportService', ['constantService', function(constants){
                
                var seconds = new Date().getTime() / 1000; //for timing the export

                var dataExport = {};


                dataExport.downloadCSV = function(volume){
                
                    volume.get(['records', 'containers']).then(function(data){
                        createCSV(data, volume);       
                    
                    });
                
                };

                function createCSV(data, volume){

                    //console.log(constants.category);
                    //console.log(constants.metric);
                    
                   
                    
                    var input = data;

                    var payload = '';

                    var containers = input.containers;
                    var records = input.records;

                    var baseHeaders = [ //these are static, tied to the volume
                        'session id',
                        'session date'
                    ];

                    
                    /*helper object and arrays*/
                    var catCounts = getCategoryCounts(containers, records);
                    var headerIncludes = makeHeaderIndex(records, constants.metric); //object of unique categories and metrics - {cat:{metric#:metricName},...}
                    var headerRef = sortHeaderIdx(makeHeaderIndex(records, constants.metric)); //sorted array version of headerIdx 
                    var colCoords = makeHeaderCoords(headerRef, catCounts); //object array that represents the headers as category,metric coords in order - [{cat:metric},...] 


                    var moreHeaders = makeHeadersText(headerRef, catCounts); //turn header index into column names

                    var header = baseHeaders.concat(moreHeaders).join(',');

                    //console.log(containers);
                    //console.log(records);

                  
                    /*create CSV body data*/
                    var body = createCSVBody(containers, records, colCoords, headerIncludes);


                    payload = header + '\n' + body;              
                        
                    createPayload(payload, volume);
                    
                }

                function createCSVBody(containers, records, headerReference, headerIndex){

                  
                  var headerCats = Object.keys(headerIndex);

                  var body = '';

                  for(var k in containers){
                    if(containers[k].top !== true && containers[k].records.length !== 0){ //exclude materials for now

                      var ssRow = [];

                      ssRow.push(containers[k].id);
                      ssRow.push(containers[k].date);
                      var recIdArr = [];
                       for (var j in containers[k].records){ //get an array of the record IDs for each container in advance
                       
                        recIdArr.push(containers[k].records[j].id);
                       } 

                       
                      var idx = 0; //create and index for the record loop so we have more control over the loop logic
                      for(var l = 0; l < headerReference.length; l++ ){

                       var cellCat = headerReference[l].category;
                       var cellMet = headerReference[l].metric.toString(); 
                        
                       

                       for (var j = idx; idx < recIdArr.length;){
                                        
                        var recID = recIdArr[j].toString();
                        var recMetrics = Object.keys(records[recID].measures).sort().reverse();

                        if(records[recID].category !== cellCat){
                             
                            ssRow.push(''); //put stuff for dev, should only be blank otherwise
                            break;
                            

                        } else {

                            if(recMetrics.length > 1){
                            
                                ssRow.push(records[recID].measures[cellMet]);
                                if(cellMet === recMetrics[recMetrics.length-1]){
                                    idx++; //only advance if we are done with all the metrics in this record
                                }
                                
                                break;
                            }else{         

                                ssRow.push(records[recID].measures[cellMet]);
                                idx++; 
                                break;
                            }    
                        }                
                        
                       } 

                      }

                    body += ssRow.join(',') + '\n';
                  }   
                }
                return body;
              }


              function createPayload(payload, volume){

                    var volTitle = volume.id + "-" + volume.name.split(' ').join('_');
                    var filename = volTitle + '.csv';
                    var uri = 'data:text/csv;charset=utf-8,' + encodeURI(payload);

                    var link = document.createElement('a');
                    link.href = uri;

                    //link.style = "visibility:hidden"; //check this on safari, throws assign on readonly error
                    link.download = filename;

                    document.body.appendChild(link);
                    link.click();
                    document.body.removeChild(link);

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

                    var catID = constants.category[item.category].id;
                    var catText = constants.category[item.category].name;

                    for(var z = 0; z < categoryCountsObj[catID]; z++){
                      if(item.metrics.length < 1){ //this is not future proof, may want to visit the data model.
                        output.push(catText); 
                      }

                      for(var m = 0; m < item.metrics.length; m++){
                        var metText = constants.metric[item.metrics[m]].name;

                        output.push(catText + ' ' + metText + ' ' + (z + 1).toString());
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
                    //console.log(tableObj);
                    return tableObj;


                }

                function sortHeaderIdx(headerIdx){

                  var newIdx = [];

                  var catKeysSorted = Object.keys(headerIdx).sort().reverse();

                  for(var v = 0; v < catKeysSorted.length; v++){

                    var metKeysSorted = Object.keys(headerIdx[catKeysSorted[v]]).sort().reverse();
                    var metricsArrSorted = [];

                    for(var m = 0; m < metKeysSorted.length; m++){

                      metricsArrSorted.push(metKeysSorted[m]);

                    }
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

                  //console.log(maxCatCounts);
                  
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
