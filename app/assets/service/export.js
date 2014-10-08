'use strict';

app.service('exportService', [function(){
                
                var dataExport = {};

                dataExport.downloadCSV = function(volume){
                
                    volume.get(['records', 'containers']).then(function(data){
                        createCSV(data);       
                    
                    });
                
                };

                function createCSV(data){

                    
                    var body = '';
                    var payload = '';
                    
                    //TODO: Headers should grab from the list of available headers and place them sorted by code
                    var headers = [
                        'id',
                        'date', 
                        'record_id',
                        'category',
                        'measure'
                    ];

                    var header = headers.join(',');

                    var array = typeof data !== 'object' ? JSON.parse(data) : data;


                    var containers = array.containers;
                    var records = array.records;

                    console.log(array.containers);
                    console.log(array.records);
                    
                    for(var k in containers){


                        for(var j in containers[k].records){
                           
                           var recID = containers[k].records[j].id;
                           var recCat = makeRecordText(records[recID].category);
                           var metricCodes = Object.keys(records[recID].measures);
                           var metric = makeMeasureText(metricCodes);
                           var metricVals = [];
                           for(var m=0; m < metricCodes.length; m++){
                                //console.log(metric[m]);
                                metricVals.push(records[recID].measures[metricCodes[m]]);                     

                           }
                           
                           
                               body += containers[k].id + ',' + 
                                       containers[k].date + ',' + 
                                       recID + ',' + 
                                       recCat + ',' + metric + "," + metricVals + '\n';

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
                }

               function makeRecordText(code){

                   var recordCodes = {
                       '-500': 'participant',
                       '-200': 'group', 
                       '-800': 'pilot',
                       '-700': 'exclusion',
                       '-400': 'condition',
                       '-300': 'task',
                       '-100': 'context'
                   };

                    return recordCodes[code];

               }

               function makeMeasureText(arr){
                  

                   var metricCodes = {
                        '-900':'ident',
                        '-590':'birthdate', //restricted
                        '-550':'race',
                        '-540':'ethnicity',
                        '-580':'gender',
                        '-140':'state',
                        '-90' :'info',
                        '-600':'description',
                        '-700':'reason',
                        '-180':'setting',
                        '-650':'summary',
                        '1': 'gestational age',
                        '-520':'disability', //restricted
                        '-510':'language',
                        '-150':'country'

                    };
                   
                   return arr.map(function(code){
                    
                    return metricCodes[code];
                           
                   }); 
               } 
                
               return dataExport;
            }

    ]);
