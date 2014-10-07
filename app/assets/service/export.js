'use strict';

app.service('exportService',
            ['$http',
            function($http){
                
                var dataExport = {};

                dataExport.downloadCSV = function(volume){
                
                    var apiResource = '../api/volume/' + volume + '?records&containers';

                    $http.get(apiResource).success(function(data){
                        createCSV(data);       
                    
                    });
                
                };

                function createCSV(data){

                var header = '';
                var body = '';
                var payload = '';

                for(var key in data){
                    if(typeof(data[key] !== 'object')){
                        header += '"' + key + '"' + ',';
                        body += '"' + data[key] + '"' + ',';

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
