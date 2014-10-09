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
                               var metricText = makeMeasureText(metricCodes); //TODO remove this, only a placeholder
                               var metricVals = [];


                              //IDEA here need to dip into the record array of measures and check each one compared to the list of available ones. go down the line and assign a val or an empty block where necessary                               
                              //need a constant object of header codes to go with the headers
                               for(var m=0; m < metricCodes.length; m++){
                                    
                                    try{
                                        if(recCode !== -300){ //TODO: Cannot use this in production, temporarily limits tasks out re: vol. 8 
                                            metricVals.push(records[recID].measures[metricCodes[m]]);  //This causes Chrome to crash on vol. 8 (so many tasks)                  
                                        }    
                                    } catch(e) {

                                      console.log(e);

                                    }
                               }
                               
                               
                               body += containers[k].id + ',' + 
                                       containers[k].date + ',' + 
                                       recID + ',' + recCode + ',' + metricText + "," + metricVals + '\n';

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

                function makeHeaderCodes(arr){
                    /* make an array of all the record and metric combinations */

                }

                function makeMeasureText(arr){
                   
                    return arr.map(function(code){ 
                       return constants.metric[code].name;
                    }); 
                } 
                
               return dataExport;
            }

    ]);
