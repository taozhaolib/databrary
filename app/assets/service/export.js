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

                    var headers = [
                        'id',
                        'date', 
                        'records'
                    ];

                    var header = headers.join(',');

                    var array = typeof data !== 'object' ? JSON.parse(data) : data;


                    var containers = array.containers;
                    var records = array.records;

                    console.log(array.containers);
                    console.log(array.records);
                    
                    for(var k in containers){

                        var contID;
                        
                        for(var j in containers[k].records){
                           
                            contID = containers[k].records[j].id;

                            body += containers[k].id + 
                                 ',' + containers[k].date + 
                                 ',' + contID + '\n';
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
                
            
               return dataExport;
            }

    ]);
