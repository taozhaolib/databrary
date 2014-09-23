'use strict';

module.directive('volumeEditOverviewForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditOverviewForm;

      function init(volume) {
	var citation = volume.citation || {};
	form.data = {
	  name: volume.name,
	  alias: volume.alias,
	  body: volume.body,
	  citation: {
	    head: citation.head,
	    title: citation.title,
	    url: citation.url,
	    authors: citation.authors && citation.authors.length ? citation.authors.slice(0) : [''],
	  }
	};
      }
      init(volume || {});

      form.setAutomatic = function (auto) {
        form.automatic = auto;
        form.validator.client({
          'citation.url': {
            tips: page.constants.message('volume.edit.citation.' + (form.automatic ? 'doi' : 'url') + '.help')
          }
        }, true);
      };

      form.save = function () {
        form.data.citation.authors = form.data.citation.authors
	  .map(function (author) {
	    return author.trim();
	  }).filter(function (author) {
	    return author !== '';
	  });

	(volume ?
          volume.save(form.data) :
          page.models.Volume.create(form.data, page.$routeParams.owner))
	  .then(function (vol) {
	    form.validator.server({});

	    form.messages.add({
	      type: 'green',
	      countdown: 3000,
	      body: page.constants.message('volume.edit.overview.success'),
	    });

	    init(vol);
	    form.$setPristine();

	    if (!volume)
	      page.$location.url(vol.editRoute());
	  }, function (res) {
	    form.validator.server(res);
	  });
      };

      form.autoDOI = function () {
        var doi = page.constants.regex.doi.exec(form.data.citation.url);

        if (!doi || !doi[1]) {
          return;
        }

        page.models.cite(doi[1])
          .then(function (res) {
            form.data.citation = res;
            form.data.name = res.title;
	    delete res.title;

            form.setAutomatic(false);

            form.messages.add({
              type: 'green',
              countdown: 3000,
              body: page.constants.message('volume.edit.autodoi.citation.success'),
            });
          }, function () {
            form.messages.add({
              type: 'red',
              countdown: 5000,
              body: page.constants.message('volume.edit.autodoi.citation.error'),
            });
          });
      };

      form.addAuthor = function () {
        form.data.citation.authors.push("");
      };

      form.addAuthorEnabled = function() {
	return form.data.citation.authors.every(function (author) {
	  return author.trim() !== '';
	});
      };

      form.removeAuthor = function (i) {
	form.data.citation.authors.splice(i, 1);
	if (!form.data.citation.authors.length)
	  form.data.citation.authors.push('');
        form.$setDirty();
      };

      var validate = {};
      ['name', 'body', 'alias', 'citation.head', 'citation.url', 'citation.year'].forEach(function (f) {
	validate[f] = {
	  tips: page.constants.message('volume.edit.' + f + '.help')
	};
      });
      form.validator.client(validate, true);
      form.setAutomatic(!volume);

      form.scrollFn = page.display.makeFloatScrollFn($('.veo-float'), $('.veo-float-floater'), 24*1.5);
      page.$w.scroll(form.scrollFn);
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/editOverview.html',
      link: link
    };
  }
]);
