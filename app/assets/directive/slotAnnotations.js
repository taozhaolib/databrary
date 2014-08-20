'use strict';

module.directive('slotAnnotations', [
	'pageService', function (page) {
		var controller = [
			'$scope', '$element', '$attrs',
			function ($scope, $element, $attrs) {
				var ctrl = page.$parse($attrs.ctrl)($scope);
				var notes = this;
				notes.list = [];

				var drawNotes = function (mode) {
					var $list = $element.find('slot-note-list');
					var $head = $element.find('slot-note-head');

					switch (mode) {
						case 'comments':
							// todo: comments
							$list.html('');
							$head.height(24);
							break;
						default:
							if (!ctrl.records[mode]) {
								ctrl.noteMode = null;
								$list.html('');
								$head.height(24);
								return;
							}

							ctrl.mode = page.constants.data.category[mode].name;

							var levels = [];

							angular.forEach(ctrl.records[mode], function (record) {
								if (!record.segment) {
									levels.push([record]);
									return;
								}

								console.log(record.segment);

								// look for the first level that has enough space
								for (var i = 0, l = levels.length; i <= l; i++) {
									if (i < l) { // try each existing level
										var m = levels[i].length;

										// skip the loop if this level has a full-width record
										if (m === 1 && !levels[i][0].segment) {
											continue;
										}

										// if this record conflicts with any of this level, break
										for (var j = 0; j < m; j++) {
											// todo: review this logic
											if ((record.segment[0] && levels[i][j].segment[1] && levels[i][j].segment[1] >= record.segment[0]) || (record.segment[1] && levels[i][j].segment[0] && levels[i][j].segment[0] <= record.segment[1])) {
												break;
											}
										}

										// all good? then push
										levels[i].push(record);
										return;
									}

									// no current level works? add a new one.
									levels.push([record]);
								}
							});
					}
				};

				$scope.$watch('ctrl.noteMode', drawNotes);

				return notes;
			}];

		//

		return {
			restrict: 'E',
			scope: false,
			templateUrl: 'slotAnnotations.html',
			controller: controller,
			controllerAs: 'notes',
		};
	}
]);
