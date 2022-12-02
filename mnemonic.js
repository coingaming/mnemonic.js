//    Mnemonic.js v. 1.1.0

//    (c) 2012-2015 Yiorgis Gozadinos, Crypho AS.
//    Mnemonic.js is distributed under the MIT license.
//    http://github.com/ggozad/mnemonic.js

// AMD/global registrations
(function (root, factory) {
    if (typeof define === 'function' && define.amd) {
// AMD. Register as an anonymous module.
        define([], function () {
            return factory();
        });
    }
    else if (typeof module !== 'undefined' && module.exports) {
        module.exports = factory();
    }
    else {
// Browser globals
        root.Mnemonic = factory();
    }
}(this, function () {

    var _crypto;

    var getRandom = function (bits) {
        var random = new Uint32Array(bits / 32);
        window[_crypto].getRandomValues(random);
        return random;
    };

    if (typeof window !== 'undefined') { //a browser
        if (window.crypto && window.crypto.getRandomValues) {
            _crypto = 'crypto';
        }
        else if (window.msCrypto && window.msCrypto.getRandomValues) {
            _crypto = 'msCrypto';
        }
    }
    else { //node.js
        var crypto = require('crypto');
        getRandom = function (bits) {
            var randomBytes = crypto.randomBytes(bits / 8),
               random = [];
            for (var i = 0; i < (bits / 32); i++) {
                random.push(randomBytes.readUInt32BE(4 * i));
            }
            return random;
        };
    }

    var Mnemonic = function (args) {
        var bits;

        if (typeof args === 'undefined' || typeof args === 'number') {
            // Create a new instance of mnemonic
            bits = args || 96;
            if (bits % 32 !== 0) {
                throw 'Can only generate 32/64/96/128 bit passwords';
            }
            this.seed = getRandom(bits);
        }
        else if (args instanceof Uint32Array) {
            this.seed = args;
        }
        return this;
    };

    Mnemonic.prototype.toHex = function () {
        var l = this.seed.length, res = '', i = 0;
        for (; i < l; i++) {
            res += ('00000000' + this.seed[i].toString(16)).substr(-8);
        }
        return res;
    };

    Mnemonic.prototype.toWords = function () {
        var i = 0, l = this.seed.length, n = Mnemonic.wc, words = [], x, w1, w2, w3;
        for (; i < l; i++) {
            x = this.seed[i];
            w1 = x % n;
            w2 = (((x / n) >> 0) + w1) % n;
            w3 = (((((x / n) >> 0) / n) >> 0) + w2) % n;
            words.push(Mnemonic.words[w1]);
            words.push(Mnemonic.words[w2]);
            words.push(Mnemonic.words[w3]);
        }
        return words;
    };

    Mnemonic.fromWords = function (words) {
        var i = 0, n = Mnemonic.wc,
           l = words.length / 3,
           seed = new Uint32Array(l),
           w1, w2, w3;

        for (; i < l; i++) {
            w1 = Mnemonic.words.indexOf(words[3 * i]);
            w2 = Mnemonic.words.indexOf(words[3 * i + 1]);
            w3 = Mnemonic.words.indexOf(words[3 * i + 2]);
            seed[i] = w1 + n * Mnemonic._mod(w2 - w1, n) + n * n * Mnemonic._mod(w3 - w2, n);
        }

        return new Mnemonic(seed);
    };

    Mnemonic.fromHex = function (hex) {
        var hexParts = hex.match(/.{1,8}/g),
           i = 0,
           l = hex.length / 8,
           seed = new Uint32Array(l),
           x;

        for (; i < l; i++) {
            x = parseInt(hexParts[i], 16);
            seed[i] = x;
        }
        return new Mnemonic(seed);
    };

    Mnemonic.wc = 1626;
    Mnemonic.words = ['copy','coat','coach','clinic','civil','chunk','chip','chief','chaos','cash','cape','cake','cable','bundle','buddy','booth','bolt','blunt','blade','bath','barn','bagel','bacon','atom','apply','ankle','amount','album','ability','like','just','love','know','never','want','time','out','there','make','look','eye','down','only','think','heart','back','then','into','about','more','away','still','them','take','thing','even','through','long','always','world','too','friend','tell','try','hand','thought','over','here','other','need','smile','again','much','been','night','ever','little','said','end','some','those','around','mind','people','leave','dream','left','turn','myself','give','nothing','really','off','before','something','find','walk','wish','good','once','place','ask','stop','keep','watch','seem','everything','wait','got','yet','made','remember','start','alone','run','hope','maybe','believe','body','after','close','talk','stand','own','each','help','home','soul','new','many','two','inside','should','true','first','mean','better','play','another','gone','change','use','wonder','someone','hair','cold','open','best','any','behind','happen','water','dark','laugh','stay','forever','name','work','show','sky','break','came','deep','door','put','black','together','upon','happy','such','great','white','matter','fill','past','please','cause','enough','touch','moment','soon','voice','anything','stare','sound','red','everyone','hide','kiss','truth','beautiful','mine','very','pass','next','forget','tree','wrong','air','mother','understand','lip','wall','memory','sleep','free','high','realize','might','skin','sweet','perfect','blue','breath','dance','against','fly','between','grow','strong','under','listen','bring','sometimes','speak','pull','person','become','family','begin','ground','real','small','father','sure','feet','rest','young','finally','land','across','today','different','guy','line','reason','reach','second','slowly','write','eat','smell','mouth','step','learn','three','floor','promise','breathe','darkness','push','earth','guess','save','song','above','along','both','color','house','almost','sorry','anymore','brother','okay','dear','game','fade','already','apart','warm','beauty','heard','notice','question','shine','began','piece','whole','shadow','secret','street','within','finger','point','morning','whisper','moon','green','story','glass','silence','since','soft','yourself','empty','shall','angel','answer','bright','path','worry','hour','drop','follow','power','half','flow','heaven','act','chance','fact','least','tired','near','quite','afraid','rise','sea','taste','window','cover','nice','trust','lot','cool','force','peace','return','blind','easy','ready','roll','rose','drive','held','music','beneath','hang','paint','emotion','quiet','clear','cloud','few','pretty','bird','outside','paper','picture','front','rock','simple','anyone','meant','reality','road','sense','waste','bit','leaf','thank','happiness','meet','smoke','truly','decide','self','age','book','form','alive','carry','escape','instead','able','ice','minute','throw','catch','leg','ring','course','goodbye','lead','poem','corner','desire','known','problem','remind','shoulder','suppose','toward','wave','drink','jump','pretend','week','human','joy','crack','grey','pray','surprise','dry','knee','less','search','caught','clean','embrace','future','king','chest','hug','remain','sat','worth','blow','final','tight','also','create','safe','cross','dress','silent','bone','fate','perhaps','class','snow','tiny','tonight','continue','control','dog','edge','mirror','month','suddenly','comfort','given','loud','quickly','gaze','plan','rush','stone','town','ignore','spirit','stood','yours','brown','build','dust','hey','kept','pay','phone','twist','although','ball','beyond','hidden','nose','taken','float','pure','somehow','wash','wrap','cheek','creature','forgotten','heat','rip','single','space','special','weak','whatever','anyway','job','choose','country','drift','echo','figure','grew','laughter','neck','yeah','disappear','foot','forward','somewhere','stomach','storm','beg','idea','lift','offer','breeze','field','five','often','simply','stuck','win','allow','enjoy','except','flower','seek','strength','calm','grin','heavy','hill','large','ocean','shoe','sigh','straight','summer','tongue','accept','everyday','exist','grass','sent','shut','surround','table','ache','brain','heal','nature','shout','sign','stain','choice','doubt','glance','glow','mountain','queen','stranger','throat','tomorrow','city','either','fish','flame','rather','shape','spin','spread','ash','distance','finish','image','imagine','important','nobody','shatter','warmth','became','feed','flesh','funny','shirt','trouble','yellow','attention','bare','bite','money','protect','amaze','appear','born','completely','fresh','friendship','gentle','probably','six','deserve','expect','grab','middle','river','thousand','weight','barely','bottle','cream','relationship','stick','test','crush','endless','itself','rule','spill','art','circle','join','kick','mask','master','passion','quick','raise','smooth','unless','wander','actually','chair','deal','favorite','gift','note','number','sweat','box','chill','clothes','mark','park','tie','animal','belong','brush','consume','dawn','forest','innocent','pen','pride','stream','thick','clay','complete','count','draw','faith','press','silver','surface','taught','teach','wet','bless','chase','climb','enter','letter','melt','metal','movie','stretch','swing','vision','beside','forgot','guide','joke','knock','plant','pour','prove','reveal','stuff','trip','wood','wrist','bother','bottom','crawl','crowd','fix','forgive','grace','loose','lucky','party','release','surely','survive','gently','grip','speed','travel','treat','written','cage','chain','conversation','date','however','interest','million','page','pink','proud','sway','themselves','winter','cup','experience','freedom','pair','pop','purpose','respect','softly','state','strange','bar','curl','dirt','excuse','lovely','order','pack','pants','pool','scene','seven','slide','among','blonde','closet','creek','eternity','gain','grade','handle','key','linger','pale','prepare','swallow','swim','wheel','won','cast','claim','college','direction','dirty','gather','hundred','lung','orange','present','swear','swirl','twice','wild','blanket','doctor','everywhere','flash','grown','knowledge','numb','pressure','radio','repeat','spend','unknown','buy','clock','early','fantasy','pound','precious','sheet','teeth','welcome','add','ahead','block','content','depth','despite','distant','marry','purple','threw','whenever','dull','easily','grasp','hospital','innocence','normal','receive','reply','rhyme','shade','someday','toe','visit','asleep','bought','center','consider','flat','hero','history','ink','muscle','mystery','pocket','reflection','shove','silently','smart','spot','train','type','view','whether','bus','energy','explain','hunger','inch','magic','mix','noise','nowhere','presence','shock','snap','spider','study','thunder','trail','admit','agree','bag','bang','bound','butterfly','cute','exactly','familiar','fold','further','pierce','reflect','scent','sharp','sink','spring','stumble','universe','wonderful','action','ancient','attempt','avoid','birthday','branch','chocolate','core','especially','focus','fruit','honest','match','palm','perfectly','pillow','roar','shift','slightly','thump','truck','tune','twenty','unable','wipe','wrote','constant','dinner','drove','egg','eternal','flight','frame','gasp','glad','hollow','motion','peer','plastic','root','screen','season','sting','strike','team','unlike','volume','warn','weird','await','awake','built','charm','crave','grant','horse','limit','message','ripple','sanity','scatter','serve','split','string','trick','blur','boat','brave','clearly','cling','connect','fist','forth','imagination','iron','jock','judge','lesson','milk','nail','ourselves','poet','possible','sail','size','snake','society','stroke','toss','trace','wise','bloom','cell','check','cost','darling','during','footstep','fragile','hallway','hardly','horizon','invisible','journey','midnight','mud','nod','pause','relax','shiver','sudden','value','youth','admire','blink','constantly','couple','curve','difference','emptiness','gotta','honor','plain','planet','recall','rub','ship','slam','soar','somebody','tightly','weather','adore','approach','bond','bread','burst','candle','coffee','cousin','desert','flutter','frozen','grand','heel','hello','language','level','movement','pleasure','powerful','random','rhythm','settle','sort','spoken','steel','tumble','aside','awkward','bee','blank','board','button','card','carefully','complain','deeply','discover','drag','effort','entire','fairy','giant','gotten','greet','illusion','jeans','leap','liquid','march','mend','nervous','nine','replace','rope','spine','apple','balance','boom','collect','demand','eventually','faint','glare','goal','group','honey','kitchen','laid','limb','machine','mere','mold','nerve','poetry','prince','rabbit','shelter','shore','shower','soothe','stair','steady','sunlight','tangle','tease','treasure','uncle','begun','bliss','canvas','cheer','claw','clutch','commit','crimson','crystal','delight','doll','existence','express','fog','football','goose','guard','illuminate','mass','math','mourn','rich','rough','skip','stir','style','support','thorn','tough','yard','yearn','yesterday','advice','appreciate','autumn','bank','beam','bowl','capture','carve','collapse','creation','dove','feather','glory','government','harsh','hop','inner','moonlight','neighbor','neither','peach','pig','praise','screw','shield','shimmer','sneak','subject','throughout','thrown','tower','twirl','wow','arrive','bathroom','bump','cease','cookie','couch','courage','dim','howl','hum','husband','led','lunch','mostly','natural','nearly','needle','nerd','peaceful','perfection','pile','price','remove','roam','sanctuary','serious','shiny','shook','tap','vain','void','wrinkle','affection','apologize','blossom','bounce','bridge','cheap','crumble','decision','descend','desperately','dig','dot','flip','frighten','heartbeat','huge','lick','odd','opinion','process','puzzle','quietly','retreat','score','sentence','separate','situation','skill','soak','square','stray','taint','task','tide','underneath','veil','whistle','anywhere','bedroom','bid','burden','careful','compare','concern','curtain','decay','defeat','describe','double','dreamer','driver','dwell','evening','flare','flicker','guitar','hungry','indeed','lace','melody','monkey','nation','object','obviously','rainbow','salt','scratch','shown','shy','stage','stun','third','tickle','afternoon','beard','boyfriend','bubble','busy','certain','chin','concrete','desk','diamond','drawn','due','freeze','frost','garden','glide','harmony','hopefully','lightning','mercy','peel','physical','position','pulse','respond','salty','sane','satisfy','savior','sheep','slept','social','sport','tuck','utter','valley','wolf','aim','alas','alter','arrow','awaken','belief','brand','ceiling','cheese','clue','confidence','connection','daily','disguise','eager','erase','essence','everytime','expression','fan','flag','flirt','foul','fur','giggle','glorious','ignorance','law','measure','mighty','muse','north','opposite','paradise','patience','patient','pencil','petal','plate','ponder','possibly','practice','slice','spell','stock','strife','strip','suit','tender','tool','trade','velvet','verse','waist','witch','bench','bold','cap','certainly','click','companion','creator','dart','delicate','determine','dish','dragon','drama','drum','dude','everybody','feast','forehead','former','fully','gas','hook','hurl','invite','juice','manage','moral','possess','raw','royal','scale','several','slight','swell','talent','tea','thread','trickle','usually','vast','weave','acid','awe','belly','blend','blush','character','common','company','creak','defense','define','depend','destination','dew','duck','dusty','engine','example','explore','freely','generation','glove','health','hurry','impossible','inhale','jaw','kingdom','mention','mist','moan','mumble','mutter','observe','ode','pattern','pie','prefer','rare','scrape','spiral','squeeze','strain','sunset','sympathy','thigh','throne','total','unseen','weary','lure','lurk','mace','malt','mama','most','move','mute','myth','navy','neon','nest','omen','omit','onto','onyx','oops','opal','oval','oven','pang','pelt','plot','plow','ploy','plug','plus','pogo','polo','pond','pony','pope','pork','posh','pout','pulp','puma','punk','purr','putt','quit','race','raft','rake','ramp','rash','ream','turf','tusk','tutu','twig','unit','used','user','veal','vest','veto','vice','visa','wake','wand','wasp','wavy','wham','wick','wife','wifi','wilt','wimp','wind','wing','wiry','woof','wool','word','yarn','yelp','yoga','yoyo','zero','zips','zone','zoom','snub','spew','spry','spud','spur','stem','stew','suds','sulk','swab','swan','taco','tall','tank','taps','that','thaw','thud','tidy','tile','till','tilt','tint','tray','trio','saga','sage','sake','same','sank','sash','send','shed','sift','silk','silo','silt','skid','slab','slaw','sled','slip','gloss','glue','gnat','goes','going','golf','gong','gooey','goofy','goon','gown','grain','grape','graph','gravy','gray','grid','grief','grill','grimy','grit','groom','grove','growl','grub','grunt','guise','gulf','gully','gulp','guru','gush','gusty','haiku','halt','hardy','haste','hasty','haunt','haven','heap','hedge','hence','herbs','hertz','hula','hulk','hull','humid','hunk','hush','icing','icon'];

    // make modulo arithmetic work as in math, not as in javascript ;)
    Mnemonic._mod = function (a, b) {
        return a - Math.floor(a / b) * b;
    };

    return Mnemonic;
}));

