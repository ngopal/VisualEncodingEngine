function initializeCache() {
    if (sessionStorage.hasOwnProperty('beento') === false) {

        var invalidPages = [1,2,9,27,31,36]; // node pages currently, need to add edge pages

        sessionStorage.setItem('beento', []);
        sessionStorage.setItem('togoto', d3.range(1,36).filter(function(n) {
                return invalidPages.indexOf(n) === -1
            })                     ); // 1, 36
        sessionStorage.setItem('tlength', 36); //need to add nodeconfig+edgeconfig length when ready
        sessionStorage.setItem('blength', 0);
        sessionStorage.setItem('status', 'incomplete');
        sessionStorage.setItem('consent', 'incomplete');
        sessionStorage.setItem('tutorial', 'incomplete');
        sessionStorage.setItem('demographic', 'incomplete');
        sessionStorage.setItem('thanks', 'incomplete');
        sessionStorage.setItem('GUID', guid());
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
                window.location.href=nextRandomPage();
            }
            else {
                if ( sessionStorage.getItem('status') === 'incomplete' ) {
                    //window.location.href="/";
                    console.log("experiment in progress");
                    //check beento here
                    var currentPageVal = getPageVal();
                    var beento = JSON.parse('['+sessionStorage.getItem('beento')+']');
                    if (beento.indexOf(currentPageVal) !== -1) {
                        var nextpage = nextRandomPage();
                        console.log("SENT TO  "+nextpage);
                        window.location.href = nextpage;
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

    if ( sessionStorage.getItem('status') === "complete" ) {
        window.location.href='/demographic';
    }

    var pageval = n;
    var beento = JSON.parse('['+sessionStorage.getItem('beento')+']');
    var togoto = JSON.parse('['+sessionStorage.getItem('togoto')+']');
    beento.push(pageval);
    togoto.splice(togoto.indexOf(parseInt(pageval)), 1);
    sessionStorage.setItem('beento', beento);
    sessionStorage.removeItem('togoto');
    sessionStorage.setItem('togoto', togoto);
    sessionStorage.setItem('tlength', togoto.length);
    sessionStorage.setItem('blength', beento.length);
    if (togoto.length === 0) {
        sessionStorage.setItem('status', 'complete');
        //sessionStorage.clear(); clear storage on exit page;
    }
};

function getRandomIntInclusive(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

function nextRandomPage() {
    var togoto = JSON.parse('['+sessionStorage.getItem('togoto')+']');
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