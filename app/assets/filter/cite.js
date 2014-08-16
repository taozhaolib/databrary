'use strict';

module.filter('cite', [
  'pageService', function (page) {
    return function (volume) {
      if (!angular.isObject(volume) || angular.isUndefined(volume.access) || angular.isUndefined(volume.name) || angular.isUndefined(volume.id)) {
        return '';
      }

      var cite = '';

      var ai = 0;
      var access = volume.access[ai];
      while (access) {
	var next = volume.access[++ai];
        if (next && (next.individual || 0) < page.permission.ADMIN)
	  next = undefined;

	if (cite !== '') {
	  cite += ', ';
	  if (!next)
	    cite += ' & ';
	}

        var parts = access.party.name.split(' ');
	cite += parts.pop();

        if (parts.length) {
	  cite += ', ';
	  do {
	    cite += parts.pop().charAt(0) + '.';
	  } while (parts.length);
        }

	access = next;
      }

      //

      cite += ' (' + page.$filter('date')(volume.creation, 'yyyy') + '). ';
      cite += volume.name + '. ';
      var retrieved = page.$filter('date')(new Date(), 'longDate');

      //

      return page.$filter('escape')(cite) + '<em>Databrary</em>. Retrieved ' + retrieved + ' from <a href="http://databrary.org/volume/' + volume.id + '">http://databrary.org/volume/' + volume.id + '</a>.';
    };
  }
]);
