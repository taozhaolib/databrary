'use strict';

app.service('exportService', ['constantService', function(constants){
                
                //for timing
                //var seconds = new Date().getTime() / 1000;

                var dataExport = {};


                dataExport.downloadCSV = function(volume){
                
                    volume.get(['records', 'containers']).then(function(data){
                        createCSV(data);       
                    
                    });
                
                };

                function createCSV(data){

                    console.log(constants.category);
                    console.log(constants.metric);
                    
                    var payload = '';
                    

                    var array = typeof data !== 'object' ? JSON.parse(data) : data;


                    var containers = array.containers;
                    var records = array.records;

                    var baseHeaders = [
                        'session id',
                        'session date'
                    ];

                    var headerIdx = makeHeaderIndex(records, constants.metric);
                    var headerRef = sortHeaderIdx(makeHeaderIndex(records, constants.metric)); //not used
                    var hRef = makeHeaderRef(headerRef);
                    var moreHeaders = makeHeadersText(headerRef);

                    console.log(hRef);

                    var header = baseHeaders.concat(moreHeaders).join(',');

                    console.log(containers);
                    console.log(records);

                  
                    /*create CSV body data*/
                    var body = createCSVBody(containers, records, hRef, headerIdx);


                    payload = header + '\n' + body;              
                        
                    createPayload(payload);
                    
                }

                function createCSVBody(containers, records, headerReference, headerIndex){

                  console.log(headerReference);
                  
                  var headerCats = Object.keys(headerIndex);

                  var body = '';

                  for(var k in containers){
                    if(containers[k].top !== true && containers[k].records.length !== 0){ //exclude materials for now

                      var ssRow = [];

                      ssRow.push(containers[k].id);
                      ssRow.push(containers[k].date);

                      for(var l = 0; l < headerReference.length; l++ ){

                       var cellCat = headerReference[l].category;
                       var cellMet = headerReference[l].metric.toString(); 
                        
                       //ssRow.push(cellCat + "|" + cellMet);

                       for (var j in containers[k].records){

                        var recID = containers[k].records[j].id;
                        if(records[recID].category === cellCat){
                          for(var m in records[recID].measures){
                           console.log(typeof(cellMet));
                           console.log(typeof(records[recID].measures[m]));
                            if (records[recID].measures[m] == cellMet){
                             ssRow.push(records[recID].measures[m]);
                             break;
                           } else {
                               ssRow.push('');
                               break;
                            } continue;
                         }
                           
                        } else {
                            ssRow.push('');
                            break;
                        }
                        
                        

                       }
                       /*for(var j in containers[k].records){

                        var recID = containers[k].records[j].id;
                        
                          
                          
                          //if (headerCats.indexOf('-800') !== -1){ //not preferred to check for this one exactly.
                            
                          //  if(records[recID].category === -800){
                          //      ssRow.push('Pilot');
                          //    } else {
                          //      ssRow.push('');

                          //  }
                          //}                  

                          var recCat = records[recID].category;

                          
                          if(recCat === cellCat){
                            for(var m in records[recID].measures){
                              if(records[recID].measures[m] === cellMet){
                               ssRow.push(records[recID].measures[cellMet]);
                             } else {

                              ssRow.push('');
                             }
                            }
                          } else {
                            ssRow.push('');
                          }


                        } */

                      }

                    body += ssRow.join(',') + '\n';
                  }   
                }
                return body;
              }


              function createPayload(payload){

                    var filename = 'download.csv';
                    var uri = 'data:text/csv;charset=utf-8,' + encodeURI(payload);

                    var link = document.createElement('a');
                    link.href = uri;

                    link.style = "visibility:hidden";
                    link.download = filename;

                    document.body.appendChild(link);
                    link.click();
                    document.body.removeChild(link);

                    //var timenow = new Date().getTime() / 1000;
                    //var timeto = (timenow - seconds);

                    //console.log("That took: " + timeto + " seconds");
                }

                /*--------------object manipulation functions-----------------*/


                function getIndex(recObj){ //DELETEME
                  var index = [];
                  for(var k in recObj){
                    if(index.indexOf(k) === -1){
                      index.push(k);
                    }

                  }

                  return index.sort().reverse();

                }

               

                function makeHeaderRef(headerIndexArr){

                  var output = [];
              
                  headerIndexArr.forEach(function(item){

                  var catID = constants.category[item.category].id;

                  if(item.metrics.length < 1){ //this is not future proof, may want to visit the data model.
                    output.push({"category": catID, "metric": ''}); 
                  }

                  item.metrics.forEach(function(m){
                    var metID = constants.metric[m].id;

                    output.push({"category": catID, "metric": metID});
                  });


                  });


                  return output;
                
                
                }

                 function makeHeadersText(headerIndexArr){
                  
                  var output = [];

                  headerIndexArr.forEach(function(item){

                    var catText = constants.category[item.category].name;

                    if(item.metrics.length < 1){ //this is not future proof, may want to visit the data model.
                      output.push(catText); 
                    }

                    item.metrics.forEach(function(m){
                      var metText = constants.metric[m].name;

                      output.push(catText + ' ' + metText);
                    });


                  });


                  return output;

                }


                function makeHeaderIndex(recObj, metrics){
                    var tableObj = {};

                    for(var key in recObj){

                      var cat = recObj[key].category;

                      tableObj[cat] = {};

                      for(var i in recObj[key].measures){

                        tableObj[cat][i] = metrics[i].name;

                      }
                    }


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
                
               return dataExport;
            }

    ]);
