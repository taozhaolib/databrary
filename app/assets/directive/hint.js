'use strict';

app.directive('hint', [
  'constantService', 'tooltipService',
  function (constants, tooltips) {
    var hints = {};

    _.each(constants.permission, function (a) {
      hints['permission-' + a] =
        constants.message('access.' + a, 'You');
      if ('access.edit.' + a + '.who' in constants.messages)
        hints['access-edit-' + a] =
          constants.message('access.edit.' + a + '.who', 'You');
    });

    _.each(constants.release, function (a) {
      hints['release-' + a] =
        constants.message('release.' + a + '.title') + ': ' + constants.message('release.' + a + '.description');
    });

    _.each(constants.format, function (a) {
      hints['format-' + a.extension] =
        a.name;
    });

    _.each(['slot'], function (a) {
      hints['action-' + a] =
        constants.message('hint.action.' + a);
    });

    _.each(['up', 'null'], function (a) {
      hints['tags-vote-' + a] =
        constants.message('tags.vote.' + a);
    });

    _.each(hints, function (hint, name) {
      new tooltips({
        live: true,
        $target: '.hint-' + name,
        message: hint
      });
    });

    var link = function ($scope, $element, $attrs) {
      $element.addClass('hint-' + $attrs.hint);
    };

    return {
      restrict: 'A',
      link: link,
    };
  }
]);
