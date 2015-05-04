'use strict'

app.directive 'spreadsheet', [
  'constantService', 'displayService', 'messageService', 'tooltipService', 'styleService', '$compile', '$templateCache', '$timeout', '$document', '$location',
  (constants, display, messages, tooltips, styles, $compile, $templateCache, $timeout, $document, $location) ->
    maybeInt = (s) ->
      if isNaN(i = parseInt(s, 10)) then s else i
    byDefault = (a,b) -> +(a > b) || +(a == b) - 1
    byNumber = (a,b) -> a-b
    byId = (a,b) -> a.id-b.id
    byType = (a,b) ->
      ta = typeof a
      tb = typeof b
      if ta != tb
        a = ta
        b = tb
      byDefault(a,b)
    byMagic = (a,b) ->
      na = parseFloat(a)
      nb = parseFloat(b)
      return 1 if na > nb
      return -1 if na < nb
      byType(a,b)

    stripPrefix = (s, prefix) ->
      if s.startsWith(prefix) then s.substr(prefix.length)

    # autovivification
    arr = (a, f) ->
      if f of a then a[f] else a[f] = []
    obj = (a, f) ->
      if f of a then a[f] else a[f] = {}
    inc = (a, f) ->
      if f of a then a[f]++ else
        a[f] = 1
        0

    pseudoMetric =
      name: # slot, asset
        id: 'name'
        name: 'name'
        display: ' '
        type: 'text'
      date: # slot
        id: 'date'
        name: 'test date'
        type: 'date'
      release: # slot
        id: 'release'
        name: 'release'
      classification: # asset
        id: 'classification'
        name: 'classification'
      excerpt: # asset
        id: 'excerpt'
        name: 'excerpt'
      id: # record
        id: 'id'
        name: 'id'
        display: ' '
        type: 'number'
        release: constants.release.PUBLIC
      age: # record
        id: 'age'
        name: 'age'
        type: 'number'
        release: constants.release.EXCERPTS
    constants.deepFreeze(pseudoMetric)
    getMetric = (m) ->
      pseudoMetric[m] || constants.metric[m]

    {
    restrict: 'E'
    scope: true
    templateUrl: 'volume/spreadsheet.html'
    controller: [
      '$scope', '$element', '$attrs',
      ($scope, $element, $attrs) ->
        Volume = $scope.volume

        Editing = $scope.editing = $attrs.edit != undefined
        Top = $scope.top = 'top' of $attrs
        Assets = 'assets' of $attrs
        ID = $scope.id = $attrs.id ? if Top then 'sst' else 'ss'
        Limit = $attrs.limit

        ###
        # We use the following types of data structures:
        #   Row = index of slot in slots and rows (i)
        #   Data[Row] = scalar value (array over Row)
        #   Slot_id = Database id of container
        #   Segment = standard time range (see type service)
        #   Record_id = Database id of record
        #   Category_id = Database id of record category (c)
        #   Count = index of record within category for slot (n)
        #   Metric_id = Database id of metric, or "id" for Record_id, or "age" (m)
        ###

        ### jshint ignore:start #### fixed in jshint 2.5.7
        Slots = (container for containerId, container of Volume.containers when Top != !container.top) # [Row] = Slot
        ### jshint ignore:end ###

        Order = if Slots.length then [0..Slots.length-1] else [] # Permutation Array of Row in display order

        Data = {}                   # [Category_id][Metric_id][Count] :: Data
        Counts = new Array(Slots.length) # [Row][Category_id] :: Count
        RecordCols = []             # [] Array over records :: {category: Category_id, metrics[]: Array of Metric_id}
        MetricCols = []             # [] Array over metrics :: {category: Category, metric: Metric} (flattened version of RecordCols)
        Depends = {}                # [Record_id][Row] :: Count

        Rows = new Array(Slots.length) # [Row] :: DOM Element tr

        TBody = $element[0].getElementsByTagName("tbody")[0]

        pseudoCategory =
          slot:
            id: 'slot'
            name: if Top then 'materials' else 'session'
            template: if Top then ['name'] else ['name', 'date', 'release']
          0:
            id: 0
            name: 'record'
            not: 'No record'
            template: [constants.metricName.ID.id]
          asset:
            id: 'asset'
            name: 'file'
            not: 'No file'
            template: ['name', 'classification', 'excerpt']
        constants.deepFreeze(pseudoCategory)
        getCategory = (c) ->
          pseudoCategory[c || 0] || constants.category[c]

        class Info
          constructor: (el) ->
            return unless el && (i = el.id) && (i = stripPrefix(i, ID+'-'))
            s = i.split '_'
            return if s.length > 1 && isNaN(@i = parseInt(s[1], 10))
            @id = i
            switch @t = s[0]
              when 'rec'
                if 3 of s
                  @n = parseInt(s[2], 10)
                  @m = parseInt(s[3], 10)
                else
                  @n = 0
                  @m = parseInt(s[2], 10)
              when 'add', 'more'
                @c = s[2]
              when 'metric'
                @m = @i
                delete @i
              when 'category'
                @c = @i
                delete @i
              #when 'asset', 'class', 'excerpt'
              #  @c = 'asset'
              #  info.a = if 2 of s then parseInt(s[2], 10) else 0

          cachedProperties =
            col: ->
              MetricCols[m] if (m = @m)?
            category: ->
              if (c = @col)?
                c.category
              else if this.hasOwnProperty('c')
                getCategory(@c)
            c: ->
              @category?.id
            metric: ->
              @col?.metric
            m: ->
              @metric.id if this.hasOwnProperty('metric')
            slot: ->
              Slots[i] if (i = @i)?
            d: ->
              Data[c].id[n][i] if (c = @c)? and (n = @n) and (i = @i)?
            record: ->
              Volume.records[d] if (d = @d)?
            asset: ->
              s.assets[d] if (s = @slot)? and (d = @d)?

          caching = (v, f) ->
            get: ->
              return unless (r = f.call(@))?
              this[v] = r
            set: (x) ->
              Object.defineProperty @, v,
                value: x
                writable: true
                configureable: true
                enumerable: true
              return

          for v, f of cachedProperties
            cachedProperties[v] = caching(v, f)

          Object.defineProperties @prototype,
            cachedProperties

        parseId = (el) ->
          info = new Info(el)
          info if info.id

        ################################# Populate data structures 

        # Fill all Data values for Row i
        populateSlot = (i) ->
          slot = Slots[i]

          # r, n
          populateMeasure = (m, v) ->
            arr(arr(r, m), n)[i] = v
            return

          count = Counts[i] = {slot: 1}
          c = 'slot'
          r = Data[c]
          n = 0
          populateMeasure('id', slot.id)
          populateMeasure('name', slot.name)
          if !slot.top || slot.date
            populateMeasure('date', slot.date)
          if !slot.top || slot.release
            populateMeasure('release', slot.release)

          for rr in slot.records
            record = rr.record
            # temporary workaround for half-built volume inclusions:
            continue unless record
            c = record.category || 0

            # populate depends:
            if record.id of Depends
              # skip duplicates:
              continue if i of Depends[record.id]
            else
              Depends[record.id] = {}

            # populate records:
            r = if c of Data then Data[c] else Data[c] = {id: []}

            # determine Count:
            n = inc(count, c)

            # populate measures:
            populateMeasure('id', record.id)
            if !Editing && 'age' of rr
              populateMeasure('age', rr.age)
            for m, v of record.measures
              populateMeasure(m, v)

            Depends[record.id][i] = n

          if Assets
            c = 'asset'
            r = Data[c]
            for assetId, asset of slot.assets
              n = inc(count, c)
              populateMeasure('id', asset.id)
              populateMeasure('name', asset.name)
              populateMeasure('classification', asset.release)
              populateMeasure('excerpt', asset.excerpt?)

          return

        # Fill MetricCols and RecordCols from records
        populateCols = ->
          MetricCols = []
          $scope.recordCols = RecordCols = Object.keys(Data).sort(byNumber).map (c) ->
            category = getCategory(c)
            if Editing
              for m in category.template
                arr(Data[c], m)
            metrics = Object.keys(Data[c]).map(maybeInt)
                    .sort(byType)
            metrics.pop() # remove 'id' (necessarily last)
            metrics = _.map metrics, getMetric
            # add back the 'id' column first if needed
            if !metrics.length
              metrics.unshift(pseudoMetric.id)
            si = MetricCols.length
            MetricCols.push.apply MetricCols, _.map metrics, (m) ->
              category: category
              metric: m
              sortable: m != pseudoMetric.id || metrics.length == 1
            l = metrics.length
            MetricCols[si].first = MetricCols[si+l-1].last = l
            {
              category: category
              metrics: metrics
              start: si
            }
          $scope.metricCols = MetricCols
          $scope.totalCols = 1 + 2*!Top + MetricCols.length + 3*Assets
          if Editing
            ### jshint ignore:start #### fixed in jshint 2.5.7
            $scope.categories = (c for ci, c of constants.category when ci not of Data)
            ### jshint ignore:end ###
            $scope.categories.sort(byId)
            $scope.categories.push(pseudoCategory[0]) unless 0 of Data
          return

        # Call all populate functions
        populate = ->
          Data = {}
          Data.slot = {id: []}
          Data.asset = {id: []} if Assets
          Depends = {}
          for s, i in Slots
            populateSlot(i)
          populateCols()
          generate()
          return

        ################################# Generate HTML
        
        # Find the text content of cell c with element t
        setCell = (c, t) ->
          el = c.lastChild
          if el && el.nodeType == 3
            c.replaceChild(t, el)
          else
            c.appendChild(t)
          return

        # Add or replace the text contents of cell c for measure/type m with value v
        generateText = (c, m, v, assumed) ->
          if m == 'name'
            a = c.insertBefore(document.createElement('a'), c.firstChild)
            #a.setAttribute('href', if editing then slot.editRoute() else slot.route())
            a.className = "session icon hint-action-slot"
            #v ?= constants.message('materials.top') if stop
          else if m == 'release'
            cn = constants.release[v || 0]
            c.className = cn + ' release icon hint-release-' + cn
            v = ''
          else if v == undefined
            c.classList.add('blank')
            v = assumed || ''
          else if m == 'classification'
            cn = constants.release[v]
            c.className = cn + ' release icon hint-release-' + cn
            v = ''
          else if m == 'excerpt'
            if v
              c.className = 'icon bullet'
            v = ''
          else
            c.classList.remove('blank')
            if m == 'id'
              c.className = 'icon ' + if Editing then 'trash' else 'bullet'
              v = ''
            else if m == 'age'
              v = display.formatAge(v)
          setCell(c, document.createTextNode(v))
          return

        # Add a td element to tr r with value c and id i
        generateCell = (r, m, v, i, assumed) ->
          td = r.appendChild(document.createElement('td'))
          if v == null
            td.className = 'null'
          else
            generateText(td, m, v, assumed)
            td.id = i
          td

        generateMultiple = (col, cols, row, i, n, t) ->
          if n == undefined
            return if t == 1
          else
            return if n < t
          td = row.appendChild(document.createElement('td'))
          td.setAttribute("colspan", cols)
          cat = col.category
          if n == undefined && t > 1
            td.appendChild(document.createTextNode(t + " " + cat.name + "s"))
            td.className = 'more'
            td.id = ID + '-more_' + i + '_' + cat.id
          else
            td.className = 'null'
            if !n || n == t
              if !n
                td.appendChild(document.createTextNode(cat.not))
              else if Editing
                td.appendChild(document.createTextNode("add " + cat.name))
              if Editing
                if 'metrics' of col && col.metrics[0].id != 'id'
                  generateCell(row, undefined, undefined, ID+'-rec_'+i+(if n then '_'+n else '')+'_'+col.start)
                  if cols > 1
                    row.appendChild(td)
                    td.setAttribute("colspan", cols-1)
                  else
                    row.removeChild(td)
                  td.className = 'null'
                else
                  td.className = 'null add'
                  td.id = ID + '-add_' + i + '_' + cat.id
          td

        # Add all the measure tds to row i for count n, record r
        generateRecord = (row, i, col, n) ->
          return unless l = col.metrics.length
          c = col.category.id
          t = Counts[i][c] || 0
          r = Data[c]
          if td = generateMultiple(col, l, row, i, n, t)
            if n == undefined
              for n in [0..t-1] by 1
                td.classList.add('ss-rec_' + r.id[n][i])
            return
          ms = col.metrics
          b = ID + '-rec_' + i + '_'
          if n == undefined
            n = 0
          else
            b += n + '_'
          for mi in [0..l-1] by 1
            m = ms[mi].id
            v = r[m][n] && r[m][n][i]
            cell = generateCell(row, m, v, b + (col.start+mi), ms[mi].assumed)
            if v != null
              ri = 'ss-rec_' + r.id[n][i]
              cell.classList.add(ri)
              cell.classList.add(ri + '_' + m)
          return

        #generateAsset = (row, i, n) ->
        #  a = assets[i]
        #  return if generateMultiple({category:pseudoCategory.asset}, 3, row, i, n, a.length)
        #  b = i
        #  if n == undefined
        #    a = a[0]
        #  else
        #    a = a[n]
        #    b += '_' + n
        #  cell = generateCell(row, 'asset', a.name, ID + '-asset_' + b)
        #  icon = cell.insertBefore(document.createElement('img'), cell.firstChild)
        #  icon.src = a.icon
        #  icon.onclick = () ->
        #    t = {asset:a.id}
        #    $location.url if Editing then Slots[i].editRoute(t) else Slots[i].route(t)
        #  icon.className = "format hint-format-" + a.format.extension
        #  generateCell(row, 'classification', a.release, ID + '-class_' + b)
        #  generateCell(row, 'excerpt', a.excerpt?, ID + '-excerpt_' + b)
        #  return

        # Fill out rows[i].
        generateRow = (i) ->
          slot = Slots[i]
          stop = slot.id == Volume.top.id
          row = if Rows[i]
              $(Rows[i]).empty()
              Rows[i]
            else
              Rows[i] = document.createElement('tr')
          row.id = ID + '_' + i
          row.data = i
          if Editing && stop
            row.className = 'top'

          name = slot.name
          if stop
            name ?= constants.message('materials.top')
          cell = generateCell(row, 'name', name, ID + '-name_' + i)
          if Editing && !stop
            a = cell.insertBefore(document.createElement('a'), cell.firstChild)
            a.className = 'trash icon'
            $(a).on 'click', (event) ->
              $scope.$apply () ->
                removeSlot(cell, i, slot)
                return
              event.stopPropagation()
              return
          a = cell.insertBefore(document.createElement('a'), cell.firstChild)
          a.setAttribute('href', if Editing then slot.editRoute() else slot.route())
          a.className = "session icon hint-action-slot"

          unless slot.top
            generateCell(row, 'date', slot.date, ID + '-date_' + i)
            generateCell(row, 'release', slot.release, ID + '-release_' + i)
          for c in RecordCols
            generateRecord(row, i, c)
          #if assets
          #  generateAsset(row, i)
          return

        # Update all age displays.
        $scope.$on 'displayService-toggleAge', ->
          for m, mi in MetricCols
            continue unless m.metric.id == 'age'
            c = m.category.id
            r = Data[c][m.metric.id]
            if expandedCat == c && Counts[expanded][c] > 1
              for n in [0..Counts[expanded][c]-1] by 1 when n of r
                e = document.getElementById(ID + '-rec_' + expanded + '_' + n + '_' + mi)
                generateText(e, 'age', r[n][expanded]) if e
            return unless 0 of r
            r = r[0]
            post = '_' + mi
            for d, i in r
              if Counts[i][c] == 1
                e = document.getElementById(ID + '-rec_' + i + post)
                generateText(e, 'age', d) if e

        # Generate all rows.
        generate = ->
          for s, i in Slots
            generateRow(i)
          fill()
          return

        ################################# Place DOM elements
        
        # Place all rows into spreadsheet.
        fill = ->
          collapse()
          delete $scope.more
          for i, n in Order
            if n >= Limit
              $scope.more = Order.length
              TBody.removeChild(Rows[i]) if Rows[i].parentNode
            else
              TBody.appendChild(Rows[i])
          return

        # Populate order based on compare function applied to values.
        sort = (values, compare) ->
          return unless values
          compare ?= byMagic
          idx = new Array(Slots.length)
          for o, i in Order
            idx[o] = i
          Order.sort (i, j) ->
            compare(values[i], values[j]) || idx[i] - idx[j]
          return

        sort(Slots.map((s) -> s.date), byDefault)
        currentSort = 'date'
        currentSortDirection = false
  
        # Sort by values, called name.
        sortBy = (key, values) ->
          if currentSort == key
            currentSortDirection = !currentSortDirection
            Order.reverse()
          else
            sort(values)
            currentSort = key
            currentSortDirection = false
          fill()
          return

        # Sort by one of the container columns.
        sortBySlot = (f) ->
          sortBy(f, Slots.map((s) -> s[f]))

        # Sort by Category_id c's Metric_id m
        sortByMetric = (col) ->
          sortBy(col, Data[col.category.id][col.metric.id][0])

        $scope.colClasses = (col) ->
          cls = []
          if typeof col == 'object'
            cls.push 'first' if col.first
            cls.push 'last' if col.last
            cls.push 'sort' if col.sortable
          else
            cls.push 'sort'
          if currentSort == col
            cls.push 'sort-'+(if currentSortDirection then 'desc' else 'asc')
          else
            cls.push 'sortable'
          cls

        ################################# Backend saving

        setFocus = undefined

        saveRun = (cell, run) ->
          messages.clear(cell)
          cell.classList.remove('error')
          cell.classList.add('saving')
          run.then (res) ->
              cell.classList.remove('saving')
              res
            , (res) ->
              cell.classList.remove('saving')
              cell.classList.add('error')
              messages.addError
                body: 'Error saving data' # FIXME
                report: res
                owner: cell
              return

        createSlot = (cell) ->
          saveRun cell, Volume.createContainer({top:Top}).then (slot) ->
            arr(slot, 'records')
            i = Slots.push(slot)-1
            Order.push(i)
            populateSlot(i)
            generateRow(i)
            TBody.appendChild(Rows[i])
            return

        saveSlot = (cell, info, v) ->
          data = {}
          data[info.t] = v ? ''
          return if info.slot[info.t] == data[info.t]
          saveRun cell, info.slot.save(data).then () ->
            generateText(cell, info.t, info.slot[info.t])
            return

        removeSlot = (cell, i, slot) ->
          # assuming we have a container
          saveRun cell, slot.remove().then (done) ->
            unless done
              messages.add
                body: constants.message('slot.remove.notempty')
                type: 'red'
                owner: cell
              return
            unedit(false)
            collapse()
            $(Rows[i]).remove()
            Slots.splice(i, 1)
            Counts.splice(i, 1)
            Rows.splice(i, 1)
            Order.remove(i)
            Order = Order.map (j) -> j - (j > i)
            populate()
            return

        saveMeasure = (cell, record, metric, v) ->
          return if record.measures[metric.id] == v
          saveRun cell, record.measureSet(metric.id, v).then (rec) ->
            rcm = Data[rec.category || 0][metric.id]
            for i, n of Depends[record.id]
              arr(rcm, n)[i] = v
              # TODO age may have changed... not clear how to update.
            l = TBody.getElementsByClassName('ss-rec_' + record.id + '_' + metric.id)
            for li in l
              generateText(li, metric.id, v, metric.assumed)
            return

        setRecord = (cell, info, record) ->
          add = ->
            if record
              info.slot.addRecord(record)
            else if record != null
              info.slot.newRecord(info.c || '')
          act =
            if info.record
              info.slot.removeRecord(info.record).then(add)
            else
              add()

          saveRun cell, act.then (record) ->
            if record
              r = record.id
              info.n = inc(Counts[info.i], info.c) unless info.record

              for m, rcm of Data[info.c]
                v = if m of record then record[m] else record.measures[m]
                if v == undefined
                  delete rcm[info.n][info.i] if info.n of rcm
                else
                  arr(rcm, info.n)[info.i] = v
              # TODO this may necessitate regenerating column headers
            else
              t = --Counts[info.i][info.c]
              for m, rcm of Data[info.c]
                for n in [info.n+1..rcm.length-1] by 1
                  arr(rcm, n-1)[info.i] = arr(rcm, n)[info.i]
                delete rcm[t][info.i] if t of rcm

            delete Depends[info.r][info.i] if info.record
            obj(Depends, r)[info.i] = info.n if record

            collapse()
            generateRow(info.i)
            expand(info) if info.n
            if record && setFocus == (i = ID+'-'+info.id) && (cell = document.getElementById(i)?.nextSibling) && (i = parseId(cell))
              select(cell, i)
            setFocus = undefined
            record

        saveAsset = (cell, info, v) ->
          data = {}
          t = info.t
          t = 'name' if t == 'asset'
          data[t] = v ? ''
          return if info.asset[t] == data[t]
          saveRun cell, info.asset.save(data).then () ->
            generateText(cell, t, info.asset[t])
            return

        ################################# Interaction

        expandedCat = undefined
        expanded = undefined

        # Collapse any expanded row.
        collapse = ->
          return if expanded == undefined
          i = expanded
          expanded = expandedCat = undefined
          row = Rows[i]
          row.classList.remove('expand')
          t = 0
          while (el = row.nextSibling) && el.data == i
            t++
            $(el).remove()

          el = row.firstChild
          while el
            el.removeAttribute("rowspan")
            el = el.nextSibling

          t

        # Expand (or collapse) a row
        expand = (info) ->
          if expanded == info.i && expandedCat == info.c
            if info.t == 'more'
              collapse()
            return
          collapse()

          expanded = info.i
          expandedCat = info.c
          row = Rows[expanded]
          row.classList.add('expand')

          max = Counts[expanded][expandedCat]
          max++ if Editing
          return if max <= 1
          next = row.nextSibling
          start = Counts[expanded][expandedCat] == 1
          col = RecordCols.find (col) -> `col.category.id == expandedCat`
          for n in [+start..max-1] by 1
            el = TBody.insertBefore(document.createElement('tr'), next)
            el.data = expanded
            el.className = 'expand'
            #if col
            generateRecord(el, expanded, col, n)
            #else
            #  generateAsset(el, expanded, n)

          max++ unless start
          el = row.firstChild
          while el
            info = new Info(el)
            if `info.c != expandedCat`
              el.setAttribute("rowspan", max)
            el = el.nextSibling
          return

        save = (cell, type, value) ->
          info = new Info(cell)
          if value == ''
            value = undefined
          else switch type
            when 'release'
              value = parseInt(value, 10)
            when 'record'
              if value == 'new'
                setRecord(cell, info)
              else if value == 'remove'
                setRecord(cell, info, null) if info.r?
              else if v = stripPrefix(value, 'add_')
                u = v.indexOf('_')
                m = constants.metric[v.slice(0,u)]
                v = v.slice(u+1)
                setRecord(cell, info).then (r) ->
                  saveMeasure(cell, r, m, v) if r
                  return
              else if !isNaN(v = parseInt(value, 10))
                if v != info.r
                  setRecord(cell, info, Volume.records[v])
              return
            when 'metric'
              if value != undefined
                arr(Data[info.c], value)
                populateCols()
                generate()
              return
            when 'category'
              if value != undefined
                arr(obj(Data, value), 'id')
                populateCols()
                generate()
              return
            when 'options'
              # force completion of the first match
              # this completely prevents people from using prefixes of options but maybe that's reasonable
              c = optionCompletions(value) if value
              value = c[0] if c?.length

          if type == 'ident'
            r = editScope.identCompleter(value)
            r.find((o) -> o.default)?.select(cell) if Array.isArray(r)
            return

          switch info.t
            when 'name', 'date', 'release'
              saveSlot(cell, info, value)
            when 'rec'
              saveMeasure(cell, info.record, info.metric, value)
            when 'asset'
              saveAsset(cell, info, value)

        editScope = $scope.$new(true)
        editScope.constants = constants
        editInput = editScope.input = {}
        editCellTemplate = $compile($templateCache.get('volume/spreadsheetEditCell.html'))
        editCell = undefined

        unedit = (event) ->
          return unless edit = editCell
          editCell = undefined
          cell = edit.parentNode
          $(edit).remove()
          return unless cell?.parentNode
          cell.classList.remove('editing')
          tooltips.clear()

          save(cell, editScope.type, editInput.value) if event != false
          cell

        recordDescription = (r) ->
          k = Object.keys(r.measures)
          if k.length
            k.sort(byNumber).map((m) -> r.measures[m]).join(', ')
          else
            '[' + r.id + ']'

        edit = (cell, info) ->
          switch info.t
            when 'name'
              return if info.slot.id == Volume.top.id
              editScope.type = 'text'
              editInput.value = info.slot.name
            when 'date'
              return if info.slot.id == Volume.top.id
              editScope.type = 'date'
              editInput.value = info.slot.date
            when 'release'
              editScope.type = 'release'
              editInput.value = (info.slot.release || 0) + ''
            when 'rec', 'add'
              if info.c == 'asset'
                # for now, just go to slot edit
                $location.url(info.slot.editRoute())
                return
              return if info.slot.id == Volume.top.id
              if info.t == 'rec' && info.metric.id == 'id'
                # trash/bullet: remove
                setRecord(cell, info, null)
                return
              if info.t == 'rec'
                m = info.metric.id
                # we need a real metric here:
                return unless typeof m == 'number'
                editInput.value = info.record?.measures[m] ? ''
                if info.col.first
                  editScope.type = 'ident'
                  editScope.info = info
                  rs = []
                  mf = (r) -> (m) -> r.measures[m]
                  for ri, r of Volume.records
                    if (r.category || 0) == info.category.id && !(ri of Depends && info.i of Depends[ri])
                      rs.push
                        r:r
                        v:(r.measures[info.metric.id] ? '').toLowerCase()
                        d:recordDescription(r)
                  editScope.records = rs.sort((a, b) -> byMagic(a.v, b.v))
                else if info.metric.options
                  editScope.type = 'options'
                  editScope.options = info.metric.options
                else if info.metric.long
                  editScope.type = 'long'
                else
                  editScope.type = info.metric.type
                break
            # when 'add', fall-through
              c = info.category
              if 'r' of info
                editInput.value = info.r + ''
              else
                editInput.value = 'remove'
              editScope.type = 'record'
              editScope.options =
                new: 'Create new ' + c.name
                remove: c.not
              for ri, r of Volume.records
                if (r.category || 0) == c.id && (!(ri of Depends && info.i of Depends[ri]) || ri == editInput.value)
                  editScope.options[ri] = r.displayName
              # detect special cases: singleton or unitary records
              for mi of Data[c.id]
                mm = constants.metric[mi]
                if !m
                  m = mm
                else if mm
                  m = null
                  break
              if m == undefined && Object.keys(editScope.options).length > 2
                # singleton: id only, existing record(s)
                delete editScope.options['new']
              else if m && m.options
                # unitary: single metric with options
                delete editScope.options['new']
                for o in m.options
                  found = false
                  for ri, r of Volume.records
                    if (r.category || 0) == c.id && r.measures[m.id] == o
                      found = true
                      break
                  editScope.options['add_'+m.id+'_'+o] = o unless found
            when 'category'
              editScope.type = 'metric'
              editInput.value = undefined
              editScope.options = []
              for mi, m of constants.metric when !(mi of Data[info.c])
                editScope.options.push(m)
              editScope.options.sort(byId)
            when 'head'
              editScope.type = 'category'
              editInput.value = undefined
              editScope.options = $scope.categories
            when 'asset'
              editScope.type = 'text'
              editInput.value = info.asset.name
            else
              return

          e = editCellTemplate editScope, (e) ->
            cell.insertBefore(editCell = e[0], cell.firstChild)
            cell.classList.add('editing')
            return
          e.on 'click', ($event) ->
            # prevent other ng-click handlers from taking over
            $event.stopPropagation()
            return

          tooltips.clear()
          $timeout ->
            input = e.find('[name=edit]')
            input.filter('input,textarea').focus().select()
            input.filter('select').focus().one('change', $scope.$lift(editScope.unedit))
            return
          return

        unselect = ->
          styles.clear()

          unedit()
          return

        $scope.$on '$destroy', unselect

        select = (cell, info) ->
          unselect()
          expand(info)
          if info.t == 'rec'
            for c, ci in cell.classList when c.startsWith('ss-rec_')
              styles.set('.' + c + '{background-color:' +
                (if c.includes('_', 7) then 'rgba(226,217,0,0.6)' else 'rgba(242,238,100,0.4)') +
                ';\n text-}')

          edit(cell, info) if Editing
          return

        $scope.click = (event) ->
          el = event.target
          return unless el.tagName == 'TD' && info = parseId(el)

          select(el, info)
          if info.hasOwnProperty('m') && MetricCols[info.m].metric.id == 'age'
            display.toggleAge()
          return

        $scope.clickSlot = ($event, t) ->
          if t
            sortBySlot(t, $event)
          else
            unselect()
          return

        editScope.unedit = ($event) ->
          unedit($event)
          false

        editSelect = () ->
          editInput.value = @text
          unedit(true)
          @text

        editScope.identCompleter = (input) ->
          info = editScope.info
          o = []
          defd = false
          add = (t, f, d) ->
            o.push
              text: t
              select: (cell) ->
                f(cell ? unedit(false))
                undefined
              default: d && !defd
            defd ||= d
          if info.r
            if input == info.record.measures[info.metric.id]
              add("Keep " + info.record.displayName,
                () -> return,
                true)
            if !input
              add("Remove " + info.record.displayName + " from this session",
                (cell) -> setRecord(cell, info, null),
                true)
          if !info.r || input && input != info.record.measures[info.metric.id]
            inputl = (input ? '').toLowerCase()
            set = (r) -> (cell) ->
              setRecord(cell, info, r)
            for r in editScope.records
              add("Use " + info.category.name + ' ' + r.d, set(r.r), r.v == inputl) if r.v.startsWith(inputl)
            v = if info.metric.options
                (x for x in info.metric.options when x.toLowerCase().startsWith(inputl))
              else
                []
            if input && !v.length
              v = [input]
            v.forEach (i) ->
              if info.r
                add("Change all " + info.record.displayName + " " + info.metric.name + " to '" + i + "'",
                  (cell) -> saveMeasure(cell, info.record, info.metric, i),
                  i == input)
              add("Create new " + info.category.name + " with " + info.metric.name + " '" + i + "'",
                (cell) -> setRecord(cell, info).then((r) ->
                  saveMeasure(cell, r, info.metric, i) if r
                  return),
                i == input)
          if o.length then o else input

        optionCompletions = (input) ->
          i = input.toLowerCase()
          (o for o in editScope.options when o.toLowerCase().startsWith(i))

        editScope.optionsCompleter = (input) ->
          match = optionCompletions(input)
          switch match.length
            when 0
              input
            when 1
              match[0]
            else
              ({text:o, select: editSelect, default: input && i==0} for o, i in match)

        editScope.next = ($event) ->
          cell = unedit($event)
          return unless cell
          setFocus = !$event.shiftKey && cell.id
          while true
            cell = if $event.shiftKey then cell.previousSibling else cell.nextSibling
            return unless cell && cell.tagName == 'TD' && info = parseId(cell)
            break unless info.t == 'rec' && info.metric.id == 'id' # skip "delete" actions
          select(cell, info)
          false

        $scope.clickAdd = ($event) ->
          unselect()
          edit($event.target, {t:'head'})
          return

        $scope.clickCategoryAdd = ($event, col) ->
          unselect()
          edit($event.target.parentNode, {t:'category',c:col.category.id}) if Editing
          return

        $scope.clickMetric = (col) ->
          sortByMetric(col) if col.sortable
          return

        $scope.clickNew = ($event) ->
          createSlot($event.target)
          return

        $scope.unlimit = ->
          Limit = undefined
          fill()

        if Editing
          $document.on 'click', ($event) ->
            if editCell && editCell.parentNode != $event.target && !$.contains(editCell.parentNode, $event.target)
              $scope.$applyAsync(unedit)
            return

        ################################# main

        $scope.refresh = ->
          unedit()
          collapse()
          populate()
          return

        populate()
        return
    ]
    }
]
