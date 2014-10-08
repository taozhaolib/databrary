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
                    ];

                    var moreHeaders = makeHeadersText(getHeaderCodes(records).sort().reverse());

                    var header = baseHeaders.concat(moreHeaders).join(',');


                    console.log(containers);
                    console.log(records);
                    
                    for(var k in containers){

                        if(containers[k].top !== true){ //exclude materials for now
                            for(var j in containers[k].records){
                               
                               var recID = containers[k].records[j].id;
                               var recCode = records[recID].category;
                               var metricCodes = Object.keys(records[recID].measures);
                               var metricText = makeMeasureText(metricCodes);
                               var metricVals = [];

                               

                                   for(var m=0; m < metricCodes.length; m++){
                                        
                                        try{
                                            if(recCode !== -300){ //temporarily limit tasks out
                                                metricVals.push(records[recID].measures[metricCodes[m]]);  //This causes Chrome to crash on vol. 8 (so many tasks)                  
                                            }    
                                        } catch(e) {

                                          console.log(e);

                                        }
                                   }
                               
                               
                                   body += containers[k].id + ',' + 
                                           containers[k].date + ',' + 
                                           recID + ',' + metricText + "," + metricVals + '\n';

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

                function getHeaderCodes(recObj){
                    
                    var headers = [];
                    for(var k in recObj){

                        if(headers.indexOf(recObj[k].category) === -1){

                            headers.push(recObj[k].category);
                        }
                    }

                    return headers;

                }

                function makeHeadersText(arr){

                    return arr.map(function(code){
                       return constants.category[code].name;
                    });

                }

                function makeMeasureText(arr){
                   
                    return arr.map(function(code){
                    
                       return constants.metric[code].name;
                           
                    }); 
                } 
                
               return dataExport;
            }

    ]);
