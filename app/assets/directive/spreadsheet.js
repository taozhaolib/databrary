'use strict';

module.directive('spreadsheet', [
  'pageService', function (page) {
    var MAXLEN = 32; // maximum number of records per category per slot
    var byNumber = function(a,b) { return a-b; };
    var byString = function(a,b) { return a>b?1:a<b?-1:0; };
    var byType = function(a,b) {
      var ta = typeof a;
      var tb = typeof b;
      if (ta > tb) return 1;
      if (ta < tb) return -1;
      if (ta == 'number') return a-b;
      return a>b?1:a<b?-1:0;
    };

    var stripPrefix = function (s, prefix) {
      return s.startsWith(prefix) ? s.substr(prefix.length) : undefined;
    };

    var parseCellId = function (id) {
      var info = {};
      var s = id.split('_');
      if ((info.type = stripPrefix(s[0], 'ss-')) === undefined ||
	  isNaN(info.i = parseInt(s[1])))
	return;
      if (info.type === 'rec') {
	info.n = parseInt(s[2]);
	info.category = parseInt(s[3]);
	if (isNaN(info.metric = parseInt(s[4])))
	  info.metric = s[4];
      }
      return info;
    };

    var controller = [
      '$scope', function ($scope) {

	var volume = $scope.volume;
	$scope.page = page;

	var editable = true || volume.permission >= page.permission.EDIT;

	var getSlot = function (slot) {
	  if ('records' in slot)
	    return slot;
	  angular.extend(slot, volume.sessions[slot.id]);
	  if (slot.segment !== undefined) {
	    slot.records = slot.records.filter(function (rec) {
	      return page.types.segmentOverlaps(slot.segment, rec.segment);
	    });
	  }
	  return slot;
	};

	/*
	 * We use the following types of data structures:
	 *   Row = index of slot in slots and rows
	 *   Data[Row] = scalar value (array over Row)
	 *   Slot_id = Database id of container
	 *   Segment = standard time range (see type service)
	 *   Record_id = Database id of record
	 *   Category_id = Database id of record category
	 *   Count = index of record within category for slot
	 *   Metric_id = Database id of metric, or "id" for Record_id, or "age"
	 */

	var slots = []; // [Row] = Slot
	angular.forEach(volume.sessions, function (s) {
	  slots.push(/*getSlot*/(s));
	});

	var count = slots.length;
	var order = Object.keys(slots); // Permutation Array of Row in display order

	var records = {}; // [Category_id][Metric_id][Count] :: Data
	var counts = new Array(count); // [Row][Category_id] :: Count
	var maxCount = new Uint8Array(count); // [Row] = counts.map(_.maxiumum)
	var recordCols = [], // [] Array over records :: {category: Category_id, metrics[]: Array of Metric_id}
	    metricCols = []; // [] Array over metrics :: {category: Category_id, metric: Metric_id} (flattened version of recordCols)
	var depends = {}; // [Record_id] :: Array of Row

	var rows = new Array(count); // [Row] :: DOM Element tr

	var ss = document.getElementById('ss');

	///////////////////////////////// Populate data structures 

	/* Fill all Data values for Row i */
	var populateSlot = function (i) {
	  var slot = slots[i];
	  
	  var r, c;
	  var populateMeasure = function (m, v) {
	    if (!(m in r))
	      r[m] = [];
	    if (!(c in r[m]))
	      r[m][c] = [];
	    r[m][c][i] = v;
	  };
	  var count = counts[i] = {};
	  var max = 0;

	  for (var ri = 0; ri < slot.records.length; ri ++) {
	    var record = volume.records[slot.records[ri].id];
	    var cat = record.category;

	    // populate records:
	    if (!(cat in records)) {
	      r = records[cat] = {id: []};
	      if (cat === page.category.participant.id)
		r.age = [];
	    } else
	      r = records[cat];

	    // determine Count:
	    if (!(cat in count)) {
	      count[cat] = 1;
	      c = 0;
	    } else
	      c = count[cat] ++;
	    max = Math.max(max, c+1);

	    // populate measures:
	    populateMeasure('id', record.id);
	    if ('age' in r)
	      populateMeasure('age', slot.records[ri].age);
	    for (var m in record.measures)
	      populateMeasure(m, record.measures[m]);

	    // populate depends:
	    if (!(record.id in depends))
	      depends[record.id] = [i];
	    else
	      depends[record.id].push(i);
	  }

	  maxCount[i] = max;
	};

	/* Fill metricCols and recordCols from records */
	var populateCols = function () {
	  metricCols = [];
	  recordCols = Object.keys(records).sort(byNumber).map(function (c) {
	    var metrics = Object.keys(records[c]).filter(function (m) {
	      // filter out 'id' and long metrics (e.g., Description)
	      return m !== 'id' && !(m in page.constants.data.metric && page.constants.data.metric[m].long);
	    });
	    if (metrics.length)
	      metrics.sort(byNumber);
	    else // add id if there's nothing else
	      metrics = ['id'];
	    metricCols.push.apply(metricCols, metrics.map(function (metric) {
	      return {
		category: c,
		metric: metric
	      };
	    }));
	    return {
	      category: c,
	      metrics: metrics
	    };
	  });
	};

	/* Call all populate* functions */
	var populate = function () {
	  for (var i = 0; i < count; i ++)
	    populateSlot(i);
	  populateCols();
	};

	///////////////////////////////// Generate HTML
	
	/* Find the text content of cell c with element t */
	var cellText = function (c, t) {
	  var el = c.lastChild;
	  if (el && el.nodeType === 3)
	    c.replaceChild(t, el);
	  else
	    c.appendChild(t);
	};

	/* Add or replace the text contents of cell c for measure/type m with value v */
	var generateText = function (c, m, v) {
	  var cls = '';
	  if (v === undefined)
	    v = '';
	  else if (m === 'age')
	    v = page.display.formatAge(v);
	  else if (m === 'consent') {
	    if (v in page.constants.data.consent) {
	      var cn = page.constants.data.consent[v].toLowerCase();
	      cls = cn + ' consent icon hint-consent-' + cn;
	      v = '';
	    }
	  } else if (typeof v === 'string' && v.length >= MAXLEN)
	    v = v.substr(0, MAXLEN) + '...';
	  cellText(c, document.createTextNode(v));
	  c.className = cls;
	};

	/* Add a td element to tr r with value c and id i */
	var generateCell = function (r, m, v, i) {
	  var td = r.appendChild(document.createElement('td'));
	  if (v === null)
	    td.className = "null";
	  else {
	    generateText(td, m, v);
	    td.id = i;
	  }
	  return td;
	};

	/* Add all the record/measure tds to row i for count n */
	var generateRecords = function (row, i, n) {
	  var count = counts[i];
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var col = metricCols[mi];
	    var c = col.category;
	    var m = col.metric;
	    var v;
	    if (n >= (count[c] || 0))
	      v = null;
	    else
	      v = records[c][m][n][i];
	    var cell = generateCell(row, m, v, 'ss-rec_' + i + '_' + n + '_' + c + '_' + m);
	    if (v !== null) {
	      var ri = 'ss-rec_' + records[c].id[n][i];
	      cell.classList.add(ri, ri + '_' + m);
	    }
	  }
	};

	/* Fill out rows[i]. Should only be called once. */
	var generateRow = function (i) {
	  var slot = slots[i];
	  var row = rows[i] = document.createElement('tr');
	  var cell;
	  row.id = 'ss_' + i;
	  row.data = i;
	  if (slot.top)
	    row.classList.add('ss-top');

	  cell = generateCell(row, 'name', slot.name, 'ss-name_' + i);
	  cell = cell.insertBefore(document.createElement('a'), cell.firstChild);
	  cell.setAttribute('href', page.router.slot(volume.id, slot));
	  cell.classList.add('link', 'icon');

	  generateCell(row, 'date', slot.date, 'ss-date_' + i);
	  generateCell(row, 'consent', slot.consent, 'ss-consent_' + i);
	  generateRecords(row, i, 0);
	  if (maxCount[i] > 1) {
	    cell = row.appendChild(document.createElement('td'));
	    cell.appendChild(document.createTextNode('+'));
	    cell.id = 'ss-exp_' + i;
	  }
	};

	/* Update all age displays. */
	var regenerateAges = function () {
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var m = metricCols[mi];
	    if (m.metric !== 'age')
	      continue;
	    var c = m.category;
	    var r = records[c][m.metric];
	    var post = '_' + c + '_age';
	    for (var i = 0; i < count; i ++) {
	      var pre = 'ss-rec_' + i + '_';
	      for (var n = 0; n < counts[i][c]; n ++) {
		var el = document.getElementById(pre + n + post);
		if (!el)
		  break;
		generateText(el, 'age', r[n][i]);
	      }
	    }
	  }
	};

	page.events.listen($scope, 'displayService-toggleAge', regenerateAges);

	/* Generate all rows. Should only be called once. */
	var generate = function () {
	  for (var i = 0; i < count; i ++)
	    generateRow(i);
	};

	///////////////////////////////// Place DOM elements

	/* Place all rows into spreadsheet. */
	var fill = function () {
	  for (var i = 0; i < count; i ++) {
	    var row = rows[order[i]];
	    collapse(order[i], row);
	    ss.appendChild(row);
	  }
	};

	/* Populate order based on compare function applied to values. */
	var sort = function (values, compare) {
	  if (!compare)
	    compare = byType;
	  order.sort(function (i, j) {
	    return compare(values[i], values[j]);
	  });
	};

	var currentSort;

	/* Sort by values, called name. */
	var sortBy = function (name, values) {
	  if (currentSort === name)
	    order.reverse();
	  else {
	    sort(values);
	    currentSort = name;
	  }
	  fill();
	};

	/* Sort by one of the container columns. */
	$scope.sortBySlot = function (f) {
	  sortBy(f, slots.map(function (s) { return s[f]; }));
	};

	/* Sort by Category_id c's Metric_id m */
	$scope.sortByMetric = function (c, m) {
	  sortBy(c + '_' + m, records[c][m][0]);
	};

	///////////////////////////////// Interaction
	
	/* Collapse a row and return true if it was previously expanded. */
	var collapse = function (i, row) {
	  var el;
	  if (!((el = row.nextSibling) && el.data === i))
	    return false;
	  do {
	    ss.removeChild(el);
	  } while ((el = row.nextSibling) && el.data === i);

	  for (el = row.firstChild; el && !el.id.startsWith("ss-rec_"); el = el.nextSibling)
	    el.removeAttribute("rowspan");
	  return true;
	};

	/* Expand (or collapse) a row */
	var expand = function (i) {
	  var row = rows[i];

	  if (collapse(i, row))
	    return;

	  var max = maxCount[i];
	  if (max <= 1)
	    return;
	  var next = row.nextSibling;
	  var el;
	  for (var n = 1; n < max; n ++) {
	    el = ss.insertBefore(document.createElement('tr'), next);
	    el.data = i;
	    generateRecords(el, i, n);
	  }

	  for (el = row.firstChild; !el.id.startsWith("ss-rec_"); el = el.nextSibling)
	    el.setAttribute("rowspan", max);
	};

	var save = function (cell, value) {
	  var info = parseCellId(cell.id);
	  var slot = slots[info.i];
	  if (value === '')
	    value = undefined;
	  switch (info.type) {
	    case 'name':
	      slot.name = value;
	      break;
	    case 'date':
	      slot.date = value = value && page.$filter('date')(value, 'yyyy-MM-dd');
	      break;
	    case 'consent':
	      slot.consent = value = parseInt(value);
	      break;
	    case 'rec':
	      /* TODO */
	      return;
	  }
	  generateText(cell, info.metric || info.type, value);
	};

	var editScope = $scope.$new(true);
	editScope.page = page;
	var editInput = editScope.input = {};
	var editCell = page.$compile(page.$templateCache.get('spreadsheetEditCell.html'));
	var editing;

	var unedit = function () {
	  var edit;
	  if (!(edit = editing))
	    return;
	  editing = undefined;
	  var cell = edit.parentNode;
	  if (!cell)
	    return;
	  var value = editInput.value;
	  cell.removeChild(edit);

	  save(cell, value);
	};

	editScope.unedit = unedit;

	var edit = function (cell, info) {
	  var slot = slots[info.i];
	  switch (info.type) {
	    case 'name':
	      editScope.type = 'text';
	      editInput.value = slot.name;
	      break;
	    case 'date':
	      editScope.type = 'date';
	      editInput.value = slot.date;
	      break;
	    case 'consent':
	      editScope.type = 'consent';
	      editInput.value = slot.consent + '';
	      break;
	    case 'rec':
	      return;
	  }
	  var edit = editCell(editScope, function (edit) {
	    cellText(cell, editing = edit[0]);
	    cell.className = "editing";
	    page.tooltips.clear();
	  });

	  page.$timeout(function () {
	    var input = edit.children('[name=edit]');
	    input.focus();
	    input.change(unedit);
	  }, 0);
	};

	var selectStyles = document.head.appendChild(document.createElement('style')).sheet;

	var unselect = function () {
	  while (selectStyles.cssRules.length)
	    selectStyles.deleteRule(0);

	  unedit();
	};

	var select = function (cell, info) {
	  unselect();

	  if (info.type === 'rec') {
	    var cl = cell.classList;
	    for (var ci = 0; ci < cl.length; ci ++) {
	      var c = cl[ci];
	      if (c.startsWith('ss-rec_'))
		selectStyles.insertRule('.' + c + '{background-color:' +
		  ((c.indexOf('_', 7) === -1) ? '#ffff88' : '#88ff88') + 
		  ';\n}', selectStyles.cssRules.length);
	    }
	  }

	  if (editable)
	    edit(cell, info);
	};

	$scope.click = function (event) {
	  var el = event.target;
	  if (el.tagName !== 'TD')
	    return;
	  if (el.parentElement.parentElement !== ss)
	    return;
	  var info = parseCellId(el.id);
	  if (!info)
	    return;

	  if (info.type === 'exp')
	    expand(info.i);
	  else if (info.metric === 'age')
	    page.display.toggleAge();
	  else
	    select(el, info);
	};

	///////////////////////////////// main

	populate();
	generate();
	fill();
	$scope.recordCols = recordCols;
	$scope.metricCols = metricCols;
      }
    ];

    return {
      restrict: 'E',
      scope: {
	volume: '=',
      },
      templateUrl: 'spreadsheet.html',
      controller: controller,
    };
  }
]);
