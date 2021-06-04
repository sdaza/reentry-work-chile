#############################################
# utility functions
# reentry work paper
# author: sebastian daza
#############################################


any_values = function(x, values) as.numeric(any(x %in% values, na.rm = TRUE))


amount_higher_zero = function(x, values) as.numeric(any(x > values))


reverse = function(x) (max(x, na.rm = TRUE) + 1) - x


renameColumns = function(dat, hash) {
    oldnames = as.vector(hash::keys(hash))
    newnames = as.vector(hash::values(hash))

    if (length(oldnames) != length(newnames)) {
        stop("Vector of names should have the same length")
    }

    setnames(dat, oldnames, newnames)
}


flag_positive_values = function(x) {
    x = na.omit(x)
    return( ifelse(
                   length(x) > 0,
                   as.numeric(any(x > 0)),
                   NA)
    )
}


lookvar  = function(dat, varnames) {
    n  = names(dat)
    nn  = list()
        for (i in 1:length(varnames)) {
            nn[[i]]  = grep(varnames[i],n)
        }

    nn  = unlist(nn)

    if ( length(nn) >0 )
        {
         r  = n[nn]
         return(r)
        }
    else
    { return("No variables found")}
}


toFirstDayOfWeek = function(date) {
    return(date - (wday(date) - 2))
}


getFirstValue = function(x) {
    return(head(na.omit(x), 1))
}


getMax = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(max(x))
    }
}


getMin = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(min(x))
    }
}


table = function (...) base::table(..., useNA = 'ifany')


cor = function (...) stats::cor(..., use = "complete.obs")


savepdf = function(file, width = 16, height = 10, mgp = c(2.2,0.45,0),
    tcl = -0.4, mar = c(3.3,3.6,1.1,1.1)) {
    fname = paste0(file, ".pdf")
    pdf(fname, width=width/2.54, height=height/2.54,
        pointsize=10)
    par(mgp = mgp, tcl = tcl, mar = mar)
}


countmis  = function(dat, vars = NULL, pct = TRUE, exclude.complete = TRUE) {

    if (is.null(vars)) {
        vars = names(dat)
    }

    mis = sort(sapply(dat[, vars, with = FALSE],
                       function(x) sum(is.na(x))), decreasing = TRUE)

    if (exclude.complete == TRUE) {
        mis = mis[ mis > 0]
    }

    if (pct == FALSE)
      { return(mis) }

    else if ( pct == TRUE ) {
        return( round(mis / nrow(dat), 3))
    }

    return(mis)
}


create_sequences = function(data, seq_variable, seq_labels = NULL, colors = NULL,
    time_index = "month_index",
    time_invariant = c("reg_folio", "month_index", "class")
    ) {

    temp = data.table::copy(data[, c(time_invariant, seq_variable), with = FALSE])
    time = as.character(order(unique(na.omit(temp$month_index))))
    temp = dcast(temp, reg_folio + class ~ month_index,
        value.var = seq_variable)
    print(paste0("Names of columns: ", paste0(names(temp), collapse = ",")))
    columns = which(names(temp) %in% time)
    print(paste0("Columns selected: ", paste0(columns, collapse = ",")))
    s1 = seqdef(temp, columns, right = NA,
        labels = seq_labels, cpal = colors)
    m1 = pstree(s1, L = 10)
    s1i = impute(m1, s1, method = "prob")
    return(s1i)
}


tranSequence = function(sequence, covs = NULL) {
    o = data.table(sequence)
    o[, id := 1:.N]
    o = melt(o, id.vars = "id", variable.name = "time", value.name = "state")
    vars = c("time", "state")
    o[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
    o[, state := state + 1]
    setorderv(o, c("id", "time"))
    print(msm::statetable.msm(state, id, data = o))
    if (!is.null(covs)) { o = merge(o, covs, by = "id", all.x = TRUE) }
    return(o)
}


create_clusters = function(seq_data, method = "HAM", norm_distance = "auto",
                           nclusters = 2:5, multichannel = FALSE,
                           cluster_labels = NULL,
                           cluster_levels = NULL) {

    list_clusters = list()

    if (multichannel == FALSE) {
        distance = seqdist(seq_data, method = method, norm = norm_distance)
    } else {
        distance = seqdistmc(channels = seq_data, method = method)
    }

    for (i in nclusters) {
        assign(paste0("c", i), wcKMedoids(distance,
                                          k = i,
                                          cluster.only = TRUE))
        assign(paste0("sil", i), wcSilhouetteObs(distance,
                                                 get(paste0("c", i)),
                                                 measure = "ASWw"))

        if (!is.null(cluster_labels)) {
            assign(paste0("c", i), factor(get(paste0("c", i)), labels = cluster_labels))
        }

        if (!is.null(cluster_levels)) {
            assign(paste0("c", i), factor(get(paste0("c", i)), levels = cluster_levels))
        }

        list_clusters[[paste0("c", i)]] = list(get(paste0("c", i)),
                                               get(paste0("sil", i))
                                               )
    }
    return(list_clusters)
}


create_plots = function(seq_data,
                        cl,
                        filepath,
                        order = "from.start",
                        method_distance = "HAM",
                        norm_distance = "auto") {

    distance = seqdist(seq_data, norm = norm_distance, method = method_distance)

    clusters = cl[[1]]

    savepdf(filepath)

    ifelse(order == "from.start",
        seqIplot(seq_data, group = group.p(clusters), sortv = "from.start"),
        seqIplot(seq_data, group = group.p(clusters), sortv = cl[[2]])
    )

    seqdplot(seq_data, group = group.p(clusters))
    seqmtplot(seq_data, group = group.p(clusters))
    seqrplot(seq_data, group = group.p(clusters),
             dist.matrix = distance, border = NA)
    TraMineRextras::seqplot.tentrop(seq_data, group = group.p(clusters))
    dev.off()
}


add_notes_table = function(tab, caption, label, align,
                           comment = "",
                           floating.environment = "table",
                           fontsize = "footnotesize",
                           arraystretch = 1.3,
                           tabcolsep = 25,
                           filename = "") {

    ptcl = print(xtable(tab, caption = caption, label = label, align = align),
             caption.placement = "top",
             floating.environment = floating.environment,
             table.placement = "htp")
    ptcl = gsub("begin\\{table\\}\\[htp\\]\\n",
                paste0("begin\\{table\\}\\[htp\\]\\\n\\\\", fontsize,
                       "\\\n\\\\setlength\\{\\\\tabcolsep\\}\\{", tabcolsep,
                       "pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{",
                       arraystretch,
                       "\\}\\\n\\\\begin\\{threeparttable\\}\\\n"),
                 ptcl)

    ptcl = gsub("end\\{tabular\\}\\n",
                paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                       comment,
                       "\\\n\\\\end{tablenotes}\\\n\\\\end{threeparttable}\\\n"),
                ptcl)

    cat(ptcl, file = filename)
}

# proportion inference
ciProp = function(dat, variable, sample = 1000, remove_missing = TRUE) {

    # total
    v = dat[[variable]]
    if (remove_missing == TRUE) v = na.omit(v)

    m = NULL
    for (i in 1:sample) {
        vs = sample(v, length(v), replace = TRUE)
        m = c(m, mean(vs))
    }

    bm = quantile(m, 0.50)
    lo = quantile(m, 0.025)
    hi = quantile(m, 0.975)
    vt = c(bm, lo, hi)
    names(vt) = c("est", "lo", "up")
    vt
}

ciPropGroup = function(dat, variable, sample = 1000, remove_missing = TRUE, groupv = NULL, glabels = NULL, 
    varlabel = NULL, test = TRUE, format = "%#.2f") {

    results = list()
    results[["Total"]] = ciProp(dat, variable, sample, remove_missing)

    p = 1.0

    if (!is.null(groupv)) {
        lgroups = unique(as.character(groupv))
        dat[, cluster := groupv]
        if (test == TRUE) {
            p = kruskal.test(get(variable) ~ as.factor(cluster), data = dat)$p.value
        }
        start = ifelse(p <= 0.05, "*", "")
        for (i in seq_along(lgroups)) {
            vg = dat[cluster == lgroups[i]]
            results[[lgroups[i]]] = ciProp(vg, variable, sample, remove_missing)
        }
        out = data.table(do.call(rbind, results), keep.rownames = TRUE)
    }

    out[, Est := paste0(sprintf(format, est), " (", sprintf(format, lo), ", ", sprintf(format, up), ")")]
    out[, Variable := ifelse(!is.null(varlabel), paste0(varlabel, start), paste0(variable, start))]
    out = dcast(out[, .(Variable, rn, Est)], Variable ~ rn, value.var = "Est")
    if (!is.null(glabels)) out = out[, c("Variable", c("Total", glabels)), with = FALSE]
    out
}


transMat = function(seq_data, labels = NULL, sample = 1000, states = NULL, columns = 1:12, format = "%#.2f") {

    d = data.table::copy(seq_data[, lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = columns])
    d[, id := 1:.N]
    tm = list()
    for (i in 1:sample) {
        s = data.table::copy(d[sample(.N, nrow(d), replace = TRUE)])
        seq = suppressMessages(seqdef(s, columns))
        tseq = suppressMessages(seqtrate(seq, sel.states = states))
        if (!is.null(labels)) { rownames(tseq) = labels; colnames(tseq) = labels } 
        tm[[i]] = tseq
    }

    m = apply(simplify2array(tm), 1:2, mean)
    mm = cbind(expand.grid(dimnames(m)), est = as.vector(m))
    lo = apply(simplify2array(tm), 1:2, quantile, prob = 0.025)
    mlo = cbind(expand.grid(dimnames(lo)), lo = as.vector(lo))
    up = apply(simplify2array(tm), 1:2, quantile, prob = 0.975)
    mup = cbind(expand.grid(dimnames(up)), up = as.vector(up))
    mseq = data.table(Reduce(function(x,y) merge(x = x, y = y, by = c("Var1", "Var2")), 
        list(mm, mlo, mup)))

    if (!is.null(labels)) mseq = mseq[order(match(Var1, labels), match(Var2, labels))]
    mseq[, Transition := paste0(Var1, " -> ", Var2)]
    mseq[, Est := paste0(sprintf(format, est), " (", sprintf('%#.2f', lo), ", ", sprintf('%#.2f', up), ")")]

    mseq[, .(Transition, Est)]
}
 

transMatGroup = function(seq, groupv = NULL, slabels = NULL, glabels = NULL, sample = 1000, states = NULL, columns = 1:12) {

    results = list()
    print("Running total")
    d = data.table(seq)
    t = transMat(d, labels = slabels, states = states, sample = sample)
    setnames(t, "Est", "Total")
    transition_order = t$Transition
    results[["total"]] = t
    d[, cluster := groupv]

    if (!is.null(groupv)) {
        groups = as.character(unique(groupv))
        for (i in seq_along(groups)) {
            print(paste0("Running group ", groups[i]))
            td = data.table::copy(d[cluster == groups[i]])
            tm = transMat(td, labels = slabels, states = states, sample = sample, columns = columns)
            setnames(tm, "Est", groups[i])
            results[[groups[i]]] = tm
        }
    }
    tab = Reduce(function(...) merge(..., all = TRUE, by = "Transition"), results)
    tab = tab[order(match(Transition, transition_order))]
    if (!is.null(groupv)) tab[, c("Transition", "Total", glabels), with = FALSE]
}


timeSpent = function(seq, states = NULL, labels = NULL, sample = 1000, columns = 1:12, prop = TRUE, format = "%#.2f") {

    d = seq[, lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = columns]
    if (is.null(states)) { states = as.character(unique(unlist(d))) } else { states = as.character(states)}
    d[, id := 1:.N]

    tm = list()
    for (i in 1:sample) {
        s = data.table::copy(d[sample(.N, nrow(d), replace = TRUE)])
        seq = suppressMessages(seqdef(s, columns))
        time = seqmeant(seq, prop = prop, serr = FALSE)[, 1]
        if (length(time) < length(states)) {
            miss = which(!states %in% names(time))
            for (k in miss) {
                time[states[k]] = 0.0
                time = time[order(names(time))]
            }
        }
        if (!is.null(labels)) { names(time) = labels }
        tm[[i]] = time
    }
    m = apply(simplify2array(tm), 1, mean)
    lo = apply(simplify2array(tm), 1, quantile, prob = 0.025)
    up = apply(simplify2array(tm), 1, quantile, prob = 0.975)

    out = data.table(est = m, lo = lo, up = up)
    if (!is.null(labels)) { out[, State := labels ]} else { out[, State := names(m)] }
    out[, Est := paste0(sprintf(format, est), " (", sprintf('%#.2f', lo), ", ", sprintf('%#.2f', up), ")")]
    out = out[, .(State, Est)]
    out

}


timeSpentGroup = function(seq, groupv = NULL, glabels = NULL, slabels = NULL,
    sample = 1000, states = NULL, prop =  TRUE, columns = 1:12) {

    results = list()
    print("Running total")
    k = data.table(seq)
    tt = timeSpent(k, labels = slabels, states = states, prop = prop, sample = sample, columns = columns)
    setnames(tt, "Est", "Total")
    results[["total"]] = tt

    k[, cluster := groupv]

    if (!is.null(groupv)) {
        groups = as.character(unique(groupv))
        for (i in seq_along(groups)) {
            print(paste0("Running group ", groups[i]))
            td = data.table::copy(k[cluster == groups[i]])
            tx = timeSpent(td, labels = slabels, states = states, sample = sample, prop = prop, columns = columns)
            setnames(tx, "Est", groups[i])
            results[[groups[i]]] = tx

        }
    }
    tab = Reduce(function(...) merge(..., all = TRUE, by = "State"), results)
    if (!is.null(groupv)) tab = tab[, c("State", "Total", glabels), with = FALSE]
    tab[order(match(State, slabels))]
}


# explore clusters
exploreSequences = function(seq_data, labs) {
    print("::::::: states ::::::::")
    print(labs)
    print("::::::: Overall state frequencies :::::::")
    print(seqstatf(seq_data))
    print("::::::: State frequencies by month :::::::")
    print(seqstatd(seq_data))
    print("::::::: Time spent on states :::::::")
    print(seqmeant(seq_data))
}


exploreCluster = function(seq_data, selected_cluster, cluster_vector, columns, 
    state, return_table = FALSE) {
    temp = data.table(
        by(seq_data,
            cluster_vector,
            seqistatd)[[selected_cluster]]
    )
    setnames(temp, names(temp), columns)
    print("::::::: Overall state frequencies :::::::")
    print(by(seq_data, cluster_vector, seqstatf)[[selected_cluster]])
    print("::::::: State frequencies by month :::::::")
    print(by(seq_data, cluster_vector, seqstatd)[[selected_cluster]])
    print("::::::: Time spent on states :::::::")
    print(by(seq_data, cluster_vector, seqmeant, prop = FALSE)[[selected_cluster]])
    print(paste0("::::::: Proportion time spent in state ", state, " :::::::"))
    print(prop.table(table(temp[[state]])))
    if (return_table) return(temp)
}







