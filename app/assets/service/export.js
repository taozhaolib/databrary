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

                    var headerIndex = getIndex(constants.category);
                    var metricIndex = getIndex(constants.metric);
                    var lonliestNumber = metricIndex.shift();

                    var moreHeaders = makeHeadersText(headerIndex);

                    var header = baseHeaders.concat(moreHeaders).join(',');

                    console.log(containers);
                    console.log(records);
                    
                    /*create CSV body data*/
                    var body = createExportBody(containers, records, headerIndex, metricIndex);
                    


                    payload = header + '\n' + body;              
                        
                    createPayload(payload);
                    
                }

                function createExportBody(containers, records, headerIndex, metricIndex){
                    

                  console.log("Header Index = " + headerIndex);
                  console.log("Metric Index = " + metricIndex);

                  var body = '';

                  for(var k in containers){

                        if(containers[k].top !== true && containers[k].records.length !== 0){ //exclude materials for now
                            
                            var ssRow = [];

                            ssRow.push(containers[k].id);
                            ssRow.push(containers[k].date);

                            for(var j in containers[k].records){
                               
                               var recID = containers[k].records[j].id;
                               var recCode = records[recID].category;
                               
                               var recordMetricIndex = getIndex(records[recID].measures);

                               if(checkIndex(records[recID].category.toString(), headerIndex) === true){

                                 for(var v in records[recID].measures){
                                   if(checkIndex(v.toString(), metricIndex) === true){
                                     ssRow.push(records[recID].measures[v]);

                                   } else {
                                     ssRow.push("no potato2");
                                   }

                                 }
                               } else {

                                ssRow.push('no potato');
                               }
     
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

                function checkIndex(item, idx){
                  var answer = false;

                  for(var x=0; x<idx.length; x++){
                    
                    if(item === idx[x]){

                      answer = true;
                      break; 

                    } else {


                    }

                  }

                  /*
                  idx.forEach(function(elem){

                    console.log('And i am: ' + typeof(elem));
                    answer = item === elem ? true : false;

                  });
                  */

                  return answer;

                }
                
               return dataExport;
            }

    ]);
