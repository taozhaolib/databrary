'use strict';

module.filter('message', [
  'constantService', (constants) -> constants.message
]);
