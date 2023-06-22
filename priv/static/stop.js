const place = decodeURI(document.location.pathname.split("/")[2]);
const maindiv = document.getElementById("main");
const titleHeader = document.getElementById("title");
const timep = document.getElementById("updatetime");
let departures;
let updated;

// On load, set title
titleHeader.innerHTML = place.charAt(0).toUpperCase() + place.slice(1);
getDepartures();
getTime();

function getTime() {
    // Update the current time
    if (updated) {
        timep.innerHTML = "Uppdaterad " + updated + " | " + (new Date()).toLocaleTimeString("sv-SE");
    } else {
        timep.innerHTML = (new Date()).toLocaleTimeString('sv-SE')
    }
    setTimeout(getTime, 250);
}

function getDepartures() {
    // Request departures from the server and show them.
    fetch("/request?stop=" + place)
    .then(response => {
        if (response.ok) return response.json();
        else throw new Error(response.status + " " + response.statusText);
    })
    .then(result => {console.log(result); update(result);})
    .catch(error => console.error(error));
}

function update(departs) {
    // Set global variable departures
    departures = departs;
    for (let dep of departs) {
        processDeps(dep.departures);
    }
    killChildren(maindiv);
    for (let dep of departs) {
        createBox(dep.title, dep.departures);
    }
    updated = (new Date()).toLocaleTimeString('sv-SE');
}

function processDeps(deps) {
    if (deps == "error") return;
    for (let dep of deps) {
        dep.time = dep.time.map(e => {
            if (e.cancelled) return "X";
            // Convert departure time to countdown
            const time = new Date(e.time) - Date.now();
            const countdown = time >= 0 ? Math.floor(time / 60000) : Math.ceil(time / 60000); // ms to min
            if (e.realtime) return (countdown == 0) ? "Nu" : countdown;
            else return "Ca " + countdown;
        });
    }
}

function killChildren(element) {
    // Remove all child elements
    while (element.firstChild) {
        element.removeChild(element.firstChild);
    }
}

function createBox(title, contents) {
    let box = document.createElement("div");
    maindiv.appendChild(box);
    // Title header
    let h3 = document.createElement("h3");
    h3.innerHTML = title;
    h3.classList.add("grouptitle");
    box.appendChild(h3);

    if (contents == "error") {
        let p = document.createElement("p");
        p.innerHTML = "Det sket sig, ursäkta.";
        box.appendChild(p);
        return;
    }

    for (let d of contents) {
        let times = d.time;
        let row = createRow(
            d.name, d.direction, times[0], 
            (times.length > 1) ? times[1]:"", 
            (times.length > 2) ? times[2]:"", 
                "background-color: " + d.bgColor + 
                "; color: " + d.fgColor + 
                "; border: 1px solid " + d.borderColor + ";"
            );
        box.appendChild(row);
    }
}

function createRow(line, dir, d1, d2, d3, style) {
    let row = document.createElement("tr");
    let td1 = document.createElement("td");
    let td2 = document.createElement("td");
    let td3 = document.createElement("td");
    let td4 = document.createElement("td");
    let td5 = document.createElement("td");
    td1.innerHTML = line;
    td2.innerHTML = dir;
    td3.innerHTML = d1;
    td4.innerHTML = d2;
    td5.innerHTML = d3;
    td1.classList.add("line");
    td2.classList.add("direction");
    td3.classList.add("time");
    td4.classList.add("time");
    td5.classList.add("time");
    td1.style = style;
    row.appendChild(td1);
    row.appendChild(td2);
    row.appendChild(td3);
    row.appendChild(td4);
    row.appendChild(td5);
    return row;
}