/* globals -app, constants, routes */
'use strict';

var app = angular.module('databraryModule', [
  'ngRoute',
  'flow'
])
  .constant('constantData', constants)
  .constant('routeData', routes.controllers);
