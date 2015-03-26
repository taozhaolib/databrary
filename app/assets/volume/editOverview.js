'use strict';

app.directive('volumeEditOverviewForm', [
  '$location', 'constantService', 'messageService', 'modelService', '$routeParams',
  function ($location, constants, messages, models, $routeParams) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditOverviewForm;

      function init(vol) {
        var volume = vol || {};
        var citation = volume.citation || {};
        form.data = {
          name: volume.name,
          alias: volume.alias,
          body: volume.body,
          citation: {
            head: citation.head,
            url: citation.url,
            year: citation.year
          },
          published: vol && vol.citation != null
        };
      }
      init(volume);
      if (!volume && $scope.owners.length) {
        form.data.owner = parseInt($routeParams.owner);
        if (!$scope.owners.some(function (o) {
            return o.id === form.data.owner;
          }))
          form.data.owner = $scope.owners[0].id;
      }

      form.save = function () {
        messages.clear(form);
        if (!form.data.published)
          form.data.citation = {head:''};
        var owner = form.data.owner;
        delete form.data.owner;

        (volume ?
          volume.save(form.data) :
          models.Volume.create(form.data, owner))
          .then(function (vol) {
            form.validator.server({});

            messages.add({
              type: 'green',
              body: constants.message('volume.edit.success'),
              owner: form
            });

            init(vol);
            form.$setPristine();

            if (!volume)
              $location.url(vol.editRoute());
          }, function (res) {
            form.validator.server(res);
          });
      };

      form.autoDOI = function () {
        messages.clear(form);
        var doi = constants.regex.doi.exec(form.data.citation.url);

        if (!doi || !doi[1]) {
          return;
        }

        models.cite(doi[1])
          .then(function (res) {
            form.data.name = res.title;
            form.data.citation = res;
            delete res.title;

            form.$setDirty();

            messages.add({
              type: 'green',
              body: constants.message('volume.edit.autodoi.citation.success'),
              owner: form
            });
          }, function () {
            messages.add({
              type: 'red',
              body: constants.message('volume.edit.autodoi.citation.error'),
              owner: form
            });
          });
      };

      var validate = {};
      ['name', 'body', 'alias', 'citation.head', 'citation.url', 'citation.year'].forEach(function (f) {
        validate[f] = {
          tips: constants.message('volume.edit.' + f + '.help')
        };
      });
      form.validator.client(validate, true);
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/editOverview.html',
      link: link
    };
  }
]);
