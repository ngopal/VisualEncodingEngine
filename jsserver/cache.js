function initializeCache() {
    if (sessionStorage.hasOwnProperty('N_beento') === false) {

        var invalidNodePages = [1, 2, 8, 23, 28];
        var invalidEdgePages = [29, 38, 39, 41];
        var acceptableNodePages = d3.range(1,29).filter(function(n) {
            return invalidNodePages.indexOf(n) === -1
        });
        var acceptableEdgePages = d3.range(29,44).filter(function(n) {
            return invalidEdgePages.indexOf(n) === -1
        });

        sessionStorage.setItem('N_beento', []);
        sessionStorage.setItem('N_togoto', acceptableNodePages ); // 1, 28
        sessionStorage.setItem('E_beento', []);
        sessionStorage.setItem('E_togoto', acceptableEdgePages ); // 29, 43
        sessionStorage.setItem('N_status', 'incomplete');
        sessionStorage.setItem('E_status', 'incomplete');
        sessionStorage.setItem('netServe', shuffle(d3.range(1,acceptableNodePages.length+acceptableEdgePages.length+1)));
        sessionStorage.setItem('netToServe', getRandNetwork());
        sessionStorage.setItem('status', 'incomplete');
        sessionStorage.setItem('consent', 'incomplete');
        sessionStorage.setItem('amt', false);
        sessionStorage.setItem('nodeprompt', 'incomplete');
        sessionStorage.setItem('edgeprompt', 'incomplete');
        sessionStorage.setItem('tutorial', 'incomplete');
        sessionStorage.setItem('popquestions', [acceptableNodePages[getRandomIntInclusive(0,acceptableNodePages.length-1)],
                                                acceptableEdgePages[getRandomIntInclusive(0,acceptableEdgePages.length-1)]  ]);
        sessionStorage.setItem('demographic', 'incomplete');
        sessionStorage.setItem('thanks', 'incomplete');
        sessionStorage.setItem('GUID', guid());

        if (window.location.href !== '/consent') {
            window.location.href = '/consent';
        }
    }
    else {
        if ( window.location.href.split('/')[3] !== "consent" && sessionStorage.getItem('consent') === 'incomplete' ) {
            window.location.href="/consent";
        }
        else if ( window.location.href.split('/')[3] === "consent" && sessionStorage.getItem('consent') === 'incomplete' ) {
            console.log("Need to obtain consent");
        }
        else if ( window.location.href.split('/')[3] === "consent" && sessionStorage.getItem('consent') === 'complete' ) {
            window.location.href='/tutorial'
        }
        else {
            if ( window.location.href.split('/')[3] !== "tutorial" && sessionStorage.getItem('tutorial') === 'incomplete' ) {
                window.location.href="/consent";
            }
            else if ( window.location.href.split('/')[3] === "tutorial" && sessionStorage.getItem('tutorial') === 'incomplete' ) {
                console.log("Need to complete tutorial");
            }
            else if ( window.location.href.split('/')[3] === "tutorial" && sessionStorage.getItem('tutorial') === 'complete' ) {
                window.location.href=nextRandomNodePage(); //Presuming we start with nodes first
            }
            else {
                if ( sessionStorage.getItem('status') === 'incomplete' ) {
                    //window.location.href="/";
                    console.log("experiment in progress");
                    //check beento here
                    var currentPageVal = getPageVal();
                    var beento;
                    if (currentPageVal > 28) {
                        beento = JSON.parse('['+sessionStorage.getItem('E_beento')+']');
                        if (beento.indexOf(currentPageVal) !== -1) {
                            var nextpage = nextRandomEdgePage();
                            console.log("SENT TO  "+nextpage);
                            window.location.href = nextpage;
                        }
                    }
                    else {
                        beento = JSON.parse('['+sessionStorage.getItem('N_beento')+']');
                        if (beento.indexOf(currentPageVal) !== -1) {
                            //if (sessionStorage.getItem("N_status") === "complete") {
                            //    window.location.href='/edgeprompt';
                            //}
                            var nextpage = nextRandomNodePage();
                            console.log("SENT TO  "+nextpage);
                            window.location.href = nextpage;
                        }
                    }
                }
                else {
                    if ( window.location.href.split('/')[3] !== "demographic" && sessionStorage.getItem('demographic') === 'incomplete' ) {
                        window.location.href="/tutorial";
                    }
                    else if ( window.location.href.split('/')[3] === "demographic" && sessionStorage.getItem('demographic') === 'incomplete' ) {
                        console.log("Need to complete demographic survey");
                    }
                    else if ( window.location.href.split('/')[3] === "demographic" && sessionStorage.getItem('demographic') === 'complete' ) {
                        window.location.href='/thanks';
                    }
                    else {
                        if ( window.location.href.split('/')[3] !== "thanks" && sessionStorage.getItem('thanks') === 'incomplete' ) {
                            window.location.href="/demographic";
                        }
                        else if ( window.location.href.split('/')[3] === "thanks" && sessionStorage.getItem('thanks') === 'incomplete' ) {
                            console.log("Need to thank participant");
                        }
                    }
                }
            }
        }
    }
}

function thissession(n) {
    //initializeCache();

    if ( sessionStorage.getItem('E_status') === "complete" ) {
        window.location.href='/demographic';
    }

    var pageval = n;
    if (pageval > 28) {
        //edge
        var beento = JSON.parse('['+sessionStorage.getItem('E_beento')+']');
        var togoto = JSON.parse('['+sessionStorage.getItem('E_togoto')+']');
        beento.push(pageval);
        togoto.splice(togoto.indexOf(parseInt(pageval)), 1);
        sessionStorage.setItem('E_beento', beento);
        sessionStorage.removeItem('E_togoto');
        sessionStorage.setItem('E_togoto', togoto);
        sessionStorage.setItem('netToServe', getRandNetwork());
        if (togoto.length === 0) {
            sessionStorage.setItem('E_status', 'complete');
            sessionStorage.setItem('status', 'complete');
            //sessionStorage.clear(); clear storage on exit page;
        }
    }
    else {
        //node
        var beento = JSON.parse('['+sessionStorage.getItem('N_beento')+']');
        var togoto = JSON.parse('['+sessionStorage.getItem('N_togoto')+']');
        beento.push(pageval);
        togoto.splice(togoto.indexOf(parseInt(pageval)), 1);
        sessionStorage.setItem('N_beento', beento);
        sessionStorage.removeItem('N_togoto');
        sessionStorage.setItem('N_togoto', togoto);
        sessionStorage.setItem('netToServe', getRandNetwork());
        if (togoto.length === 0) {
            sessionStorage.setItem('N_status', 'complete');
            window.location.href='/edgeprompt';
            //sessionStorage.clear(); clear storage on exit page;
        }
    }
};

function getRandomIntInclusive(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

function nextRandomNodePage() {
    var togoto = JSON.parse('['+sessionStorage.getItem('N_togoto')+']');
    if (togoto.length === 0) {
        return '/edgeprompt';
    }
    var randsel = togoto[getRandomIntInclusive(0,togoto.length-1)];
    return '/page'+randsel;
}

function nextRandomEdgePage() {
    var togoto = JSON.parse('['+sessionStorage.getItem('E_togoto')+']');
    if (togoto.length === 0) {
        return '/demographic';
    }
    var randsel = togoto[getRandomIntInclusive(0,togoto.length-1)];
    return '/page'+randsel;
}

function getPageVal() {
    var pageval;
    if (window.location.href.split('/page')[1] === undefined) {
        pageval = 1;
    }
    else {
        pageval = parseInt(window.location.href.split('/page')[1]);
    }
    return pageval;
}

function guid() {
    function s4() {
        return Math.floor((1 + Math.random()) * 0x10000)
            .toString(16)
            .substring(1);
    }
    return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
        s4() + '-' + s4() + s4() + s4();
}

function shuffle(array) {
    var currentIndex = array.length, temporaryValue, randomIndex;

    // While there remain elements to shuffle...
    while (0 !== currentIndex) {

        // Pick a remaining element...
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;

        // And swap it with the current element.
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }

    return array;
}

function getRandNetwork() {
    var netServes = JSON.parse('['+sessionStorage.getItem('netServe')+']');
    var toreturn = netServes.pop();
    sessionStorage.setItem('netServe', netServes);
    return 'rn'+toreturn;
}