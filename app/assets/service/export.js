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
                    var body = '';
                    var payload = '';
                    

                    var array = typeof data !== 'object' ? JSON.parse(data) : data;


                    var containers = array.containers;
                    var records = array.records;

                    var baseHeaders = [
                        'session id',
                        'session date', 
                        'record id',
                        'category'
                    ];

                    var headerIndex = getIndex(constants.category);
                    var metricIndex = getIndex(constants.metric);

                    var lonliestNumber = metricIndex.shift();

                    console.log("Header Index = " + headerIndex);
                    console.log("Metric Index = " + metricIndex);

                    var moreHeaders = makeHeadersText(headerIndex);

                    var header = baseHeaders.concat(moreHeaders).join(',');

                    console.log(containers);
                    console.log(records);
                    
                    for(var k in containers){

                        if(containers[k].top !== true){ //exclude materials for now
                            for(var j in containers[k].records){
                               alert(containers[k].records[j].id);
                               
                               var recID = containers[k].records[j].id;
                               var recCode = records[recID].category;

                               var colVals = [];
                               
                               var recordMetricIndex = getIndex(records[recID].measures);

                               for(var z=0; z < headerIndex.length; z++){
                                  //console.log(recCode);
                                  //console.log(headerIndex[z]);

                                  if(recCode == headerIndex[z]){
                                    
                                    for(var m in records[recID].measures){
                                      console.log(containers[k].id);
                                      for(var y=0; y < metricIndex.length; y++){
                                        if(m == metricIndex[y]){
                                          colVals.push(records[recID].measures[m]);
                                        } else {

                                          //colVals.push('');
                                        }

                                      }
                                    }


                                  } else {
                                    colVals.push('');


                                  }

                               }

                               //colVals, at this point, should be the same length as moreHeaders.
                               if(colVals.length === moreHeaders.length){
                                console.log("good job, all values in");
                                console.log(colVals);

                               } else {
                                console.log("uh, oh, not all values in");
                                console.log(colVals);

                               }
                               
                              /* this 
                               var metricCodes = Object.keys(records[recID].measures);
                               var metricText = makeMeasureText(metricCodes); //TODO remove this, only a placeholder
                               var metricVals = [];


                               for(var m=0; m < metricCodes.length; m++){
                                    
                                    try{
                                        if(recCode !== -300){ //TODO: Cannot use this in production, temporarily limits tasks out re: vol. 8 
                                            metricVals.push(records[recID].measures[metricCodes[m]]);  //This causes Chrome to crash on vol. 8 (so many tasks)                  
                                        }    
                                    } catch(e) {

                                      console.log(e);

                                    }
                               }
                               endthis*/
                               
                               body += containers[k].id + ',' + 
                                       containers[k].date + ',' + 
                                       recID + ',' + recCode /*+ ',' + metricText + "," + metricVals*/ + '\n';

                            }
                      }

                    }


                    payload = header + '\n' + body;              
                        
                    createPayload(payload);
                    
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


                function getIndex(recObj){
                  var index = [];
                  for(var k in recObj){
                    if(index.indexOf(k) === -1){
                      index.push(k);
                    }

                  }

                  return index.sort().reverse();

                }

                function makeHeadersText(arr){
                    
                    var output = [];

                    arr.forEach(function(item){

                      var colPrefix = constants.category[item].name;
                      var childArr = []; 

                      for(var i=0; i<constants.category[item].template.length; i++){
                        childArr.push(constants.category[item].template[i]);

                      }

                      childArr = childArr.sort().reverse();
                      
                      
                      childArr.forEach(function(child){
                         var colSuffix = constants.metric[child].name;

                         output.push(colPrefix + ' ' + colSuffix);

                      });

                    });

                    
                    return output;


                }

                function makeMeasureText(arr){
                   
                    return arr.map(function(code){ 
                       return constants.metric[code].name;
                    }); 
                } 
                
               return dataExport;
            }

    ]);
