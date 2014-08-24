'use strict';

module.directive('spreadsheet', [
  'pageService', function (page) {
    var MAXLEN = 16; // maximum number of records per category per slot
    function maybeInt(s) {
      var i = parseInt(s);
      return isNaN(i) ? s : i;
    }
    function byNumber(a,b) { return a-b; }
    function byType(a,b) {
      var ta = typeof a;
      var tb = typeof b;
      if (ta > tb) return 1;
      if (ta < tb) return -1;
      return a>b?1:a<b?-1:0;
    }
    function byMagic(a,b) {
      var na = parseFloat(a);
      var nb = parseFloat(b);
      if (na > nb) return 1;
      if (na < nb) return -1;
      return byType(a,b);
    }

    function stripPrefix(s, prefix) {
      return s.startsWith(prefix) ? s.substr(prefix.length) : undefined;
    }

    function parseCellId(id) {
      var info = {};
      var s = id.split('_');
      if ((info.t = stripPrefix(s[0], 'ss-')) === undefined ||
	  isNaN(info.i = parseInt(s[1])))
	return;
      if (info.t === 'rec') {
	info.n = parseInt(s[2]);
	info.m = parseInt(s[3]);
      }
      if (info.t === 'add') {
	info.c = parseInt(s[2]);
      }
      return info;
    }

    var SAVE = false; // disable commit to server for testing

    var pseudoMetrics = {
      id: {
	id: 'id',
	name: 'id',
	type: 'number',
	classification: page.classification.PUBLIC
      },
      age: {
	id: 'age',
	name: 'age',
	type: 'number',
	classification: page.classification.SHARED
      },
    };
    Object.freeze(pseudoMetrics);

    function getMetric(m) {
      return pseudoMetrics[m] || page.constants.data.metric[m];
    }

    var controller = [
      '$scope', function ($scope) {

	var volume = $scope.volume;
	$scope.page = page;

	var editable = !SAVE || volume.permission >= page.permission.EDIT;

	function getSlot(slot) {
	  if ('records' in slot)
	    return slot;
	  angular.extend(slot, volume.sessions[slot.id]);
	  if (slot.segment !== undefined) {
	    slot.records = slot.records.filter(function (rec) {
	      return page.types.segmentOverlaps(slot.segment, rec.segment);
	    });
	  }
	  return slot;
	}

	/*
	 * We use the following types of data structures:
	 *   Row = index of slot in slots and rows (i)
	 *   Data[Row] = scalar value (array over Row)
	 *   Slot_id = Database id of container
	 *   Segment = standard time range (see type service)
	 *   Record_id = Database id of record
	 *   Category_id = Database id of record category (c)
	 *   Count = index of record within category for slot (n)
	 *   Metric_id = Database id of metric, or "id" for Record_id, or "age" (m)
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
	    metricCols = []; // [] Array over metrics :: {category: Category, metric: Metric} (flattened version of recordCols)
	var depends = {}; // [Record_id][Row] :: Count

	var rows = new Array(count); // [Row] :: DOM Element tr

	var table = document.getElementById('spreadsheet-table');

	///////////////////////////////// Populate data structures 

	/* Fill all Data values for Row i */
	function populateSlot(i) {
	  var slot = slots[i];
	  
	  var r, n;
	  function populateMeasure(m, v) {
	    if (!(m in r))
	      r[m] = [];
	    if (!(n in r[m]))
	      r[m][n] = [];
	    r[m][n][i] = v;
	  }
	  var count = counts[i] = {};
	  var max = 0;

	  for (var ri = 0; ri < slot.records.length; ri ++) {
	    var record = volume.records[slot.records[ri].id];
	    var c = record.category;

	    // populate depends:
	    if (record.id in depends) {
	      // skip duplicates:
	      if (i in depends[record.id])
		continue;
	    } else
	      depends[record.id] = {};

	    // populate records:
	    if (c in records)
	      r = records[c];
	    else {
	      r = records[c] = {id: []};
	      if (c === page.category.participant.id)
		r.age = [];
	    }

	    // determine Count:
	    if (c in count)
	      n = count[c] ++;
	    else {
	      count[c] = 1;
	      n = 0;
	    }
	    max = Math.max(max, n+1);

	    // populate measures:
	    populateMeasure('id', record.id);
	    if ('age' in r)
	      populateMeasure('age', slot.records[ri].age);
	    for (var m in record.measures)
	      populateMeasure(m, record.measures[m]);

	    depends[record.id][i] = n;
	  }

	  maxCount[i] = max;
	}

	/* Fill metricCols and recordCols from records */
	function populateCols() {
	  metricCols = [];
	  recordCols = Object.keys(records).sort(byNumber).map(function (c) {
	    var category = page.constants.data.category[c];
	    var metrics = Object.keys(records[c]).filter(function (m) {
	      // filter out 'id' and long metrics (e.g., Description)
	      return m !== 'id' && !getMetric(m).long;
	    }).map(maybeInt);
	    if (metrics.length)
	      metrics.sort(byType);
	    else // add id if there's nothing else
	      metrics = ['id'];
	    var si = metricCols.length;
	    metricCols.push.apply(metricCols, metrics.map(function (m, i) {
	      return {
		category: category,
		metric: getMetric(m)
	      };
	    }));
	    var l = metrics.length;
	    metricCols[si].first = metricCols[si+l-1].last = l;
	    return {
	      category: c,
	      metrics: metrics
	    };
	  });
	}

	/* Call all populate* functions */
	function populate() {
	  for (var i = 0; i < count; i ++)
	    populateSlot(i);
	  populateCols();
	}

	///////////////////////////////// Generate HTML
	
	/* Find the text content of cell c with element t */
	function setCell(c, t) {
	  var el = c.lastChild;
	  if (el && el.nodeType === 3)
	    c.replaceChild(t, el);
	  else
	    c.appendChild(t);
	}

	/* Add or replace the text contents of cell c for measure/type m with value v */
	function generateText(c, m, v, assumed) {
	  if (assumed)
	    c.classList.remove('assumed');
	  if (v === undefined)
	  {
	    if (assumed) {
	      v = assumed;
	      c.classList.add('assumed');
	    } else
	      v = '';
	  }
	  else if (m === 'age')
	    v = page.display.formatAge(v);
	  else if (m === 'consent') {
	    if (v in page.constants.data.consent) {
	      var cn = page.constants.data.consent[v].toLowerCase();
	      c.className = cn + ' consent icon hint-consent-' + cn;
	      v = '';
	    }
	  } else if (typeof v === 'string' && v.length >= MAXLEN)
	    v = v.substr(0, MAXLEN) + '...';
	  setCell(c, document.createTextNode(v));
	}

	/* Add a td element to tr r with value c and id i */
	function generateCell(r, m, v, i, assumed) {
	  var td = r.appendChild(document.createElement('td'));
	  if (v === null)
	    td.className = 'null';
	  else {
	    generateText(td, m, v, assumed);
	    td.id = i;
	  }
	  return td;
	}

	function generateAdd(r, i, c, l) {
	  var td = r.appendChild(document.createElement('td'));
	  td.setAttribute("colspan", l);
	  td.appendChild(document.createTextNode("add " + c.name));
	  td.id = 'ss-add_' + i + '_' + c.id;
	  td.className = 'add';
	}

	/* Add all the record/measure tds to row i for count n */
	function generateRecords(row, i, n, edit) {
	  var count = counts[i];
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var col = metricCols[mi];
	    var c = col.category.id;
	    var m = col.metric.id;
	    var t = count[c] || 0;
	    var v;
	    if (edit && n === t) {
	      if (col.first)
		generateAdd(row, i, col.category, col.first);
	      continue;
	    }
	    if (n >= t)
	      v = null;
	    else
	      v = records[c][m][n][i];
	    var cell = generateCell(row, m, v, 'ss-rec_' + i + '_' + n + '_' + mi, col.metric.assumed);
	    if (v !== null) {
	      var ri = 'ss-rec_' + records[c].id[n][i];
	      cell.classList.add(ri, ri + '_' + m);
	    }
	    if (col.first && n === 0 && t > 1)
	      cell.classList.add('more');
	  }
	}

	/* Fill out rows[i]. Should only be called once. */
	function generateRow(i) {
	  var slot = slots[i];
	  var row = rows[i] = document.createElement('tr');
	  var cell;
	  row.id = 'ss_' + i;
	  row.data = i;

	  cell = generateCell(row, 'name', slot.name, 'ss-name_' + i);
	  cell = cell.insertBefore(document.createElement('a'), cell.firstChild);
	  cell.setAttribute('href', page.router.slot(volume.id, slot));
	  cell.classList.add('link', 'icon');

	  generateCell(row, 'date', slot.date, 'ss-date_' + i);
	  generateCell(row, 'consent', slot.consent, 'ss-consent_' + i);
	  generateRecords(row, i, 0, editable && slot.id !== volume.top.id);
	}

	/* Update all age displays. */
	function regenerateAges() {
	  for (var mi = 0; mi < metricCols.length; mi ++) {
	    var m = metricCols[mi];
	    if (m.metric.id !== 'age')
	      continue;
	    var c = m.category.id;
	    var r = records[c][m.metric.id][0];
	    var post = '_0_' + mi;
	    for (var i = 0; i < count; i ++) {
	      if (counts[i][c]) {
		var el = document.getElementById('ss-rec_' + i + post);
		if (el)
		  generateText(el, 'age', r[i]);
	      }
	    }
	    if (expanded !== undefined)
	      r = records[c][m.metric.id];
	      for (var n = 0; n < counts[expanded][c]; n ++) {
		generateText(
		    document.getElementById('ss-rec_' + expanded + '_' + n + '_' + mi),
		    'age', r[n][expanded]);
	      }
	  }
	}

	page.events.listen($scope, 'displayService-toggleAge', regenerateAges);

	/* Generate all rows. Should only be called once. */
	function generate() {
	  for (var i = 0; i < count; i ++)
	    generateRow(i);
	}

	///////////////////////////////// Place DOM elements
	
	var tbody_main = document.getElementById('ss');
	var tbody_top = document.getElementById('ss-top');

	function tbody(i) {
	  return slots[i].top ? tbody_top : tbody_main;
	}

	/* Place all rows into spreadsheet. */
	function fill() {
	  collapse();
	  for (var o = 0; o < count; o ++) {
	    var i = order[o];
	    tbody(i).appendChild(rows[i]);
	  }
	}

	/* Populate order based on compare function applied to values. */
	function sort(values, compare) {
	  if (!compare)
	    compare = byMagic;
	  order.sort(function (i, j) {
	    return compare(values[i], values[j]);
	  });
	}

	var currentSort;

	/* Sort by values, called name. */
	function sortBy(name, values) {
	  if (currentSort === name)
	    order.reverse();
	  else {
	    sort(values);
	    currentSort = name;
	  }
	  fill();
	}

	/* Sort by one of the container columns. */
	$scope.sortBySlot = function (f) {
	  sortBy(f, slots.map(function (s) { return s[f]; }));
	};

	/* Sort by Category_id c's Metric_id m */
	$scope.sortByMetric = function (c, m) {
	  sortBy(c + '_' + m, records[c][m][0]);
	};

	///////////////////////////////// Backend saving
	
	function saveError() {
	  // TODO
	}

	function saveSlot(i, f, v) {
	  function success() {
	    var slot = slots[i];
	    slot[f] = v;
	    // volume.sessions[slot.id][f] = v; // may be redundant, but that's fine
	    var el = document.getElementById('ss-' + f + '_' + i);
	    if (el)
	      generateText(el, f, v);
	  }
	  if (SAVE) {
	    var data = {};
	    data[f] = v;
	    page.models.slot.update(slots[i], data, success, saveError);
	  } else success();
	}

	function saveMeasure(r, m, v) {
	  function success() {
	    var rec = volume.records[r];
	    rec.measures[m] = v;
	    var rcm = records[rec.category][m];
	    angular.forEach(depends[r], function (n, i) {
	      rcm[n][i] = v;
	    });
	    var l = table.getElementsByClassName('ss-rec_' + r + '_' + m);
	    var a = page.constants.data.metric[m].assumed;
	    for (var li = 0; li < l.length; li ++)
	      generateText(l[li], m, v, a);
	  }
	  if (SAVE) {
	    page.models.record.measureUpdate({id:r,metric:m}, {datum:v}, success, saveError);
	  } else success();
	}

	///////////////////////////////// Interaction
	
	var expanded;

	/* Collapse any expanded row. */
	function collapse() {
	  if (expanded === undefined)
	    return;
	  var i = expanded;
	  expanded = undefined;
	  var row = rows[i];
	  var el;
	  var p = row.parentNode; // tbody(i)
	  if (!((el = row.nextSibling) && el.data === i))
	    return false;
	  do {
	    p.removeChild(el);
	  } while ((el = row.nextSibling) && el.data === i);

	  for (el = row.firstChild; el && !el.id.startsWith("ss-rec_"); el = el.nextSibling)
	    el.removeAttribute("rowspan");
	  return true;
	}

	/* Expand (or collapse) a row */
	function expand(i) {
	  var row = rows[i];

	  if (expanded === i)
	    return;
	  collapse();
	  expanded = i;

	  var max = maxCount[i];
	  var edit = editable && slots[i].id !== volume.top.id;
	  if (edit)
	    max ++;
	  if (max <= 1)
	    return;
	  var next = row.nextSibling;
	  var el;
	  var p = tbody(i);
	  for (var n = 1; n < max; n ++) {
	    el = p.insertBefore(document.createElement('tr'), next);
	    el.data = i;
	    generateRecords(el, i, n, edit);
	  }

	  for (el = row.firstChild; !el.id.startsWith("ss-rec_"); el = el.nextSibling)
	    el.setAttribute("rowspan", max);
	}

	function save(cell, type, value) {
	  var info = parseCellId(cell.id);
	  if (value === '')
	    value = undefined;
	  else switch (type) {
	    case 'choose':
	      var c = 'c' in info ? page.constants.data.category[info.c] : metricCols[info.m].category;
	      if (value === 'new') {
		/* TODO: create new record and add to slot */
	      } else if (value === 'remove') {
		/* TODO: remove this record from slot */
	      } else {
		var ri = parseInt(value);
		/* TODO: replace current record with ri on slot
		 * If ri is current record, switch to edit mode? */
	      }
	      return;
	    case 'date':
	      value = page.$filter('date')(value, 'yyyy-MM-dd');
	      break;
	    case 'number':
	      value = parseFloat(value);
	      break;
	    case 'consent':
	      value = parseInt(value);
	      break;
	  }

	  switch (info.t) {
	    case 'name':
	    case 'date':
	    case 'consent':
	      return saveSlot(info.i, info.t, value);
	    case 'rec':
	      var col = metricCols[info.m];
	      return saveMeasure(records[col.category.id].id[info.n][info.i], col.metric.id, value);
	  }
	}

	var editScope = $scope.$new(true);
	editScope.page = page;
	var editInput = editScope.input = {};
	var editCell = page.$compile(page.$templateCache.get('spreadsheetEditCell.html'));
	var editing;

	function unedit(event) {
	  var edit;
	  if (!(edit = editing))
	    return;
	  editing = undefined;
	  var cell = edit.parentNode;
	  if (!cell)
	    return;
	  cell.removeChild(edit);
	  cell.classList.remove('editing');
	  page.tooltips.clear();

	  if (event && event.type === 'change')
	    save(cell, editScope.type, editInput.value);
	}

	editScope.unedit = unedit;

	function edit(cell, info) {
	  var slot = slots[info.i];
	  switch (info.t) {
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
	      var col = metricCols[info.m];
	      var ri = records[col.category.id].id[info.n][info.i];
	      if (!col.first) {
		var metric = col.metric;
		var m = metric.id;
		/* we need a real metric here: */
		if (typeof m !== 'number')
		  return;
		editInput.value = volume.records[ri].measures[m];
		if (metric.options) {
		  editScope.type = 'select';
		  editScope.options = metric.options;
		} else
		  editScope.type = metric.type;
		break;
	      }

	      var c = col.category;
	      editInput.value = ri + '';
	      /* falls through */
	    case 'add':
	      if (c === undefined) {
		c = page.constants.data.category[info.c];
		editInput.value = 'remove';
	      }
	      editScope.type = 'choose';
	      editScope.options = {
		'new':'Create new ' + c.name,
		'remove':'No ' + c.name
	      };
	      angular.forEach(volume.records, function (r, ri) {
		if (r.category === c.id && (!(info.i in depends[ri]) || ri === editInput.value)) {
		  editScope.options[ri] = page.types.recordName(r);
		}
	      });
	      break;
	    default:
	      return;
	  }
	  var e = editCell(editScope, function (e) {
	    cell.insertBefore(editing = e[0], cell.firstChild);
	    cell.classList.add('editing');
	  });

	  page.tooltips.clear();
	  page.$timeout(function () {
	    var input = e.children('[name=edit]');
	    input.focus();
	    input.change(unedit);
	  }, 0);
	}

	var selectStyles = document.head.appendChild(document.createElement('style')).sheet;

	function unselect() {
	  while (selectStyles.cssRules.length)
	    selectStyles.deleteRule(0);

	  unedit();
	}

	function select(cell, info) {
	  unselect();

	  expand(info.i);

	  if (info.t === 'rec') {
	    var cl = cell.classList;
	    for (var ci = 0; ci < cl.length; ci ++) {
	      var c = cl[ci];
	      if (c.startsWith('ss-rec_'))
		selectStyles.insertRule('.' + c + '{background-color:' +
		  (!c.contains('_', 7) ? '#ffff88' : '#88ff88') + 
		  ';\n}', selectStyles.cssRules.length);
	    }
	  }

	  if (editable)
	    edit(cell, info);
	}

	$scope.click = function (event) {
	  var el = event.target;
	  if (el.tagName !== 'TD')
	    return;
	  var info = parseCellId(el.id);
	  if (!info)
	    return;

	  select(el, info);
	  if ('m' in info && metricCols[info.m].metric.id === 'age')
	    page.display.toggleAge();
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
