'use strict';

module.filter('cite', [
  'pageService', function (page) {
    return function (volume) {
      if (!angular.isObject(volume) || angular.isUndefined(volume.access) || angular.isUndefined(volume.name) || angular.isUndefined(volume.id)) {
        return '';
      }

      var names = [];

      angular.forEach(volume.access, function (access) {
        if (angular.isUndefined(access.individual) || access.individual < page.permission.ADMIN) {
          return;
        }

        var parts = access.party.name.split(' '),
          name = parts.pop();

        if (parts.length > 0) {
          name += ', ' + parts.map(function (n) {
            return n.charAt(0);
          }).join('. ') + '.';
        }

        names.push(name);
      });

      names = names.join(', ');

      //

      var created = page.$filter('date')(new Date(volume.creation), 'yyyy');
      var retrieved = page.$filter('date')(new Date(), 'longDate');

      //

      return page.$filter('escape')(names) + ', ' + created + '. ' + page.$filter('escape')(volume.name) + '. <em>Databrary</em>. Retrieved ' + retrieved + ' from <a href="http://databrary.org/volume/' + volume.id + '" title="' + page.$filter('escape')(volume.name) + '">http://databrary.org/volume/' + volume.id + '</a>.';
    };
  }
]);
