

%when the downloaded brisnet DRF file is loaded into MATLAB turn the ".DRF"
%to ".txt"

%download file close to start of first race then analyze so that this can
%take out the scratches and shave updated odds.

FileID = 'GPX0522.txt'; %enter file name here


warning('off','MATLAB:textio:io:UnableToGuessFormat'); %turns off annoying warning about datetime stuff that is irrelevant to my procedure
Tableread =  readtable(FileID,'Delimiter',{',',';'},'ConsecutiveDelimitersRule','split', 'MissingRule', 'fill'); %reads table as table
Table = readmatrix(FileID,'Delimiter',{',',';'},'ConsecutiveDelimitersRule','split', 'MissingRule', 'fill'); %reads only numbers in table but exact same format and locations as readtable, easier for my calcualtions
Tablecell = table2cell(Tableread);

Tracktemp = char(FileID); %gets track name from fileid name
TrackName = string(Tracktemp(1:2));

Rdatetemp0 = string(Table(1,2)); %Detects and gives Year of race
Rdatetemp1 = char(Rdatetemp0);
RYtemp2 = Rdatetemp1(1:4);
RY = str2double(RYtemp2);
RMtemp2 = Rdatetemp1(5:6);
RM = str2double(RMtemp2);
RDtemp2 = Rdatetemp1(7:8);
RD = str2double(RDtemp2);
RaceDayofYeartemp = datetime(RY,RM,RD);
RaceDayofYear = day(RaceDayofYeartemp,'dayofyear');%used to calculate work rest days

Racetrack = Tablecell(1,1); %string, track of todays race for this file

 %RaceCondition = input('Conditions? (FM,MD,FST): ');

 %ENTER SCRATCHES HERE
NumRaces = Table(end,3); %detects number of races
[Rows,~] = size(Table);
%ENTER IN ORDER FRO FIRST RACE TO LAST AND LEAST TO GREATEST
Scratches = zeros(NumRaces,Rows); %pre-allocating
% Scratches(R #, H #) = 1; Follow this format to plug in horse scratches
%according to race and post position
% Scratches(1,3) = 1;
% Scratches(1,14) = 1;
% Scratches(1,15) = 1;
% Scratches(3,7) = 1;
% Scratches(5,1) = 1;
% Scratches(5,13) = 1;
% Scratches(5,14) = 1;
% Scratches(5,15) = 1;
% Scratches(6,9) = 1;
% Scratches(7,4) = 1;
% Scratches(7,8) = 1;
% Scratches(7,9) = 1;
% Scratches(8,13) = 1;
% Scratches(8,14) = 1;
% Scratches(8,15) = 1;
% Scratches(8,16) = 1;
% Scratches(9,2) = 1;
% Scratches(9,6) = 1;
% Scratches(10,8) = 1;
% Scratches(11,13) = 1;
% Scratches(11,14) = 1;
% Scratches(11,15) = 1;
% Scratches(11,16) = 1;




Phi = ((1+sqrt(5))/2); %the golden ratio... just in case

Central = zeros(Rows, 34);  %centralized table for all horse numbers, six is how many pieces of data will be there



NumHorsesPerRaceTable = zeros(NumRaces,5); %pre-allocating

NumPPRacesPerHorse = zeros(Rows,5); %pre-allocating to put both what race the horse is in, what #in the race, how many pps it has, and its chronological number, # of workouts

  for k = 1:NumRaces %separates each race
      
      horsevec = zeros(1,Rows); %preallocates vect to determine number of horses per race
      
      for j = 1:Rows %goes through every row to detect Race #
         
         if Table(j,3) == k %goes through each race seperately, counting on saving results/variables at the end of each loop 

          horsevec(j) = 1;
          
          horsenumber = horsevec(j) * Table(j,4);
          
          NumPPRacesPerHorse(j,3:4) = [k,horsenumber]; %tells what post position each horse is
          
         end 
         
         NumHorsesPerRace = sum(horsevec); %adds up all of the horses (ones) which = num of horses in that race, should work with scratches
         
        NumofPPRaces = 0;

       for l = 256:266 %PP race dates listed start at 256, max is 10 races i think but we put 269 just in case, and they come as numbers which are all over 20 mil
                if Table(j,l) >= 20000000
                     NumofPPRaces = NumofPPRaces +1; % counts the number of dates which = num of pp races, luckily the dates are numbers in the 20 million range so they stand out.
                end
       end
       
       NumofWorkouts = 0;
       
       for work = 102:113
           if Table(j,work) >= 20000000
               NumofWorkouts = NumofWorkouts + 1;
           end
       end
         
         NumPPRacesPerHorse(j,1:2) = [j,NumofPPRaces];
         
         NumPPRacesPerHorse(j,5) = (NumofWorkouts);
         
         NumHorsesPerRaceTable(k,1:2) = [k,NumHorsesPerRace]; %filling in table with num of horses wrt race
         
      end
      
          
  end
  
  Central(:,1:2) = NumPPRacesPerHorse(:,3:4); %these tell the rase and horse # in race

  %cleans the bris speed data
  for d = 1:Rows
      if isnan(Table(d,1178))
          Table(d,1178) = 0; %if there is a NaN for the speed figure, then set to 0, this will allow me to take average later on
      end
       if isnan(Table(d,1179))
          Table(d,1179) = 0; 
       end
       if isnan(Table(d,1181))
          Table(d,1181) = 0; 
       end
  end
  
  
  
     %creates endign value for each race
   for q = 2:NumRaces
       NumHorsesPerRaceTable(1,3) = NumHorsesPerRaceTable(1,2);
       NumHorsesPerRaceTable(1,4) = 1;
       NumHorsesPerRaceTable(1,5) = 1;
       NumHorsesPerRaceTable(q,3) = NumHorsesPerRaceTable(q,2) + NumHorsesPerRaceTable(q-1,3); %end points
       NumHorsesPerRaceTable(q,4) = NumHorsesPerRaceTable(q,3) - NumHorsesPerRaceTable(q,2); %startpoints
       NumHorsesPerRaceTable(q,5) = NumHorsesPerRaceTable(q,4) + 1; %adds one to the end point so that later in this script i can create index startpoints
       
   end
   
   startadvantagetable = ones(Rows,1); %preallocating
   startadvantagetablebonus = 1.5; %multiplied by the early margin speed gives bonus to horses where they have empty stalls next to them thus more room off the gate
   

   for s = 1:NumRaces %goes through each race
       for t = 1:Rows %goes through each horse, although not this many hroses in eac race, but will still work this way
           if Scratches (s,t) == 1 %if horse is scratches according to my input
               NumHorsesPerRaceTable(s,2) = NumHorsesPerRaceTable(s,2) - 1; %subtracts one horse from the total race
               HorseNumTemp1 = NumHorsesPerRaceTable(s,5); %gets first horse in each race
               HorseNumTemp2 = HorseNumTemp1 + (t-1); %adds the number for the horse in the race, but subtracts one becuase it inclusive counting
               startadvantagetable(HorseNumTemp2,1) = startadvantagetablebonus; %gives bonus to horses that have no horses next ti them off start
              
           end
       end
   end
   
   
   
   
   
   %THis loop takes out the scratches form the race population table so
   %that the race population calculatioj isnt messed up, also the full
   %scratches loop is located at the just before the relative competition
   % loop
   
   
   %preallocating, gets the value for all coefficients where each  value
   %will equal one
   CurrentCoef1 = zeros(Rows,9); %nine is the number of variale in that section, each column is one
   PrepCoef1 = zeros(Rows,8);
   PPcoef1 = zeros(Rows,15); %create a smaller temporary table that will adjust to each horse and take the avg of those values then put into larger chart for each horse
   Workcoef1 = zeros(Rows,4);
   
   
   for horse = 1:Rows %has to  be up here so that these constants dont become redefined each time a horse has 0 pps
       %PP calculations will be done in this for loop, then the needed results
       %will be indexed in a central matrix that can store all of the horses
       %data
       
       
       RaceSurface = cell2mat(Tablecell(horse,7)); %character
       RaceClass = cell2mat(Tablecell(horse,9)); %character, switch case, check data structure online for code
       
      if RaceSurface == 'D'
           
           %PP SECTION (Central column 6)
           FINALppscoreexp = 1/2.7; %CURRENTLY using outpefromnce as a coefficient, but still applies
           restdaysexp = .3;
           raceoddsexp = 1; %1/sqrt(raceplacefinish) is applied here so no real need for raceoddsexp
           NormalizedBrisSpeedexp = .2; %-sqrt(x)
           racevalueexp = .01;
           raceweightcarriedexp = .01;
           racepostpositionexp = .2;
           startadvantagetablebonus = 0; %ADJUST ABOVE THE SCRATCH LOOP
           EarlyMarginSpeedexp = .1; %-atan(x), because more negative the better
           MidMarginSpeedexp = .4; %-atan(x)
           LateMarginSpeedexp = 0; %-atan(x)
           raceconditionbonusnum = 1;%should be >1 %gives a bonus to races where the conditions were ideal for what this model is for, fast, dry
           %^^^ only benefits race accuracy for races where conditoons were fast
           %and dry be careful what races i use
           NormalizedLateMarginSpeedexp = 2.5;
           MarginTrendexp = .01; % -atan(x)
           AverageOutperformanceexp = .395;
           Weightoflastraceinpp = .5; %represents a percentage %this is the that i want each race to linearly decrease to in terms of importance
           ppraceweightifnotsurface = .81 ; %percentage that past races not on same surface should count
           racedistboost = .35; % + 1   gives a boost to the horses score if they ran well in a race at the smae distance as todya, and if didnt run well then it will give experience points
           
           
           
           %WORKOUT SECTION (Central Column 5)
           FINALworksscoreexp = 1/2; % because number is so high
           worksconsistencyaverageexp = 1; %e^-x, also not sure whether std or mean is better
           worksavgexp = 1; %sqrt(x)
           workdistexp = .5; %not sure if to use sqrt(x) here
           numworksperdayexp = .05; %sqrt(x)
           WrkWeightIfnoPPs = .2; %added to FINALworksscoreexp, to increase wrkout weight if there us no pps
           
           
           %CURRENT SECTION (Central column 4)
           FINALcurrentscoreexp = 1/3.5;
           MorningOddsexp = .1; %DIFFERENT FROM PUBLIC ODDS!!!!!!!, set by a person so prefer to be as low as possible
           HorseLife1stwinsurfaceexp = 2;
           HorseCurYearTop3Recordexp = .05;
           HorseAtDistTop3Percentageexp = .1;
           HorseAtTrackTop3Percentageexp = 1.2;
           Brisspeedonsurfaceexp = .2;
           HorseLifeTop3winsurfaceexp = .05;
           HorseAtDist1stpercentageexp = .1;
           Populationfinishpredicitonexp = 5;
           CurrWeightIfnoPPs = .1;
           %   RacePurseexp = .005;
           %   RaceClaimingPriceexp = .005;
           %   ClaimingPriceHorseexp = .005;
           %ADD IN THE OTHER EARNINGS, TAYLORED TO CONDITIONS AND SURFACE, AS WELL
           %AS PUT IN THE OTHER WIN PERCENTAGES
           
           %PREPERATION SECTION (Central column 10)
           FINALpreperationscoreexp = 1/5;
           HorseAgeinMonthsexp = .3;
           Trainertop3winexp = 1;
           Jockeytop3winexp = 1;
           RaceRestDaysexp = 1.5; %e^1/sqrt(x)
           HorseWeigthCarriedexp = .05;
           PostPositionexp = .1;
           PrepweightIfnoPPs = .2;
           HorseBlinkerstatusexp = 1; % if horse wore blnkers its a negative
           WorkRestDaysexp = 1; %e^1/x, numbers will be naturally smaller than race rest days due to the missing sq root
           RestDaysRaceMinbenefit = 39; %the minimium number of rest days from a race that a horse benefits from
           RestDaysRaceMaxbenefit = 60; %the maximium number of rest days from a race that a horse benefits from
           RestDaysWorkMinBenefit = 14; %the minimium number of rest days from a workout that a horse benefits from
           RestDaysWorkMaxBenefit = 70; %the minimium number of rest days from a workout that a horse benefits from
           
           
           
           %these three must always add up to 1!!!
           Baseline = .7;%this is the first tier that all that horses account for in the race, Acts like a pyramid as the horses that get closer to the horse get weighed more
           FirstTier = .2; % this applies to the closes four horses
           SecondTier = .1; % applies to the closest two horses
           
           
       else %for turf
           
           %PP SECTION (Central column 6)
           FINALppscoreexp = 1/2.7; %CURRENTLY using outpefromnce as a coefficient, but still applies
           restdaysexp = .3;
           raceoddsexp = 1; %1/sqrt(raceplacefinish) is applied here so no real need for raceoddsexp
           NormalizedBrisSpeedexp = .2; %-sqrt(x)
           racevalueexp = .01;
           raceweightcarriedexp = .01;
           racepostpositionexp = .2;
           startadvantagetablebonus = 0; %ADJUST ABOVE THE SCRATCH LOOP
           EarlyMarginSpeedexp = .5; %-atan(x), because more negative the better
           MidMarginSpeedexp = .5; %-atan(x)
           LateMarginSpeedexp = 1; %-atan(x)
           raceconditionbonusnum = 1;%should be >1 %gives a bonus to races where the conditions were ideal for what this model is for, fast, dry
           %^^^ only benefits race accuracy for races where conditoons were fast
           %and dry be careful what races i use
           NormalizedLateMarginSpeedexp = 1;
           MarginTrendexp = .01; % -atan(x)
           AverageOutperformanceexp = .395;
           Weightoflastraceinpp = .5; %represents a percentage %this is the that i want each race to linearly decrease to in terms of importance
           ppraceweightifnotsurface = .81 ; %percentage that past races not on same surface should count
           racedistboost = .2; % + 1   gives a boost to the horses score if they ran well in a race at the smae distance as todya, and if didnt run well then it will give experience points
           
           
           
           %WORKOUT SECTION (Central Column 5)
           FINALworksscoreexp = 1/2; % because number is so high
           worksconsistencyaverageexp = 2; %e^-x, also not sure whether std or mean is better
           worksavgexp = 1; %sqrt(x)
           workdistexp = .5; %not sure if to use sqrt(x) here
           numworksperdayexp = .05; %sqrt(x)
           WrkWeightIfnoPPs = .2; %added to FINALworksscoreexp, to increase wrkout weight if there us no pps
           
           
           %CURRENT SECTION (Central column 4)
           FINALcurrentscoreexp =  1/3.5;
           MorningOddsexp = .1; %DIFFERENT FROM PUBLIC ODDS!!!!!!!, set by a person so prefer to be as low as possible
           HorseLife1stwinsurfaceexp = 2;
           HorseCurYearTop3Recordexp = .05;
           HorseAtDistTop3Percentageexp = .1;
           HorseAtTrackTop3Percentageexp = 1.2;
           Brisspeedonsurfaceexp = .2;
           HorseLifeTop3winsurfaceexp = .05;
           HorseAtDist1stpercentageexp = .1;
           Populationfinishpredicitonexp = 5;
           CurrWeightIfnoPPs = .1;
           %   RacePurseexp = .005;
           %   RaceClaimingPriceexp = .005;
           %   ClaimingPriceHorseexp = .005;
           %ADD IN THE OTHER EARNINGS, TAYLORED TO CONDITIONS AND SURFACE, AS WELL
           %AS PUT IN THE OTHER WIN PERCENTAGES
           
           %PREPERATION SECTION (Central column 10)
           FINALpreperationscoreexp = 1/6;
           HorseAgeinMonthsexp = .3;
           Trainertop3winexp = 1;
           Jockeytop3winexp = 1;
           RaceRestDaysexp = 1.5; %e^1/sqrt(x)
           HorseWeigthCarriedexp = .05;
           PostPositionexp = .1;
           PrepweightIfnoPPs = .2;
           HorseBlinkerstatusexp = 1; % if horse wore blnkers its a negative
           WorkRestDaysexp = 1; %e^1/x, numbers will be naturally smaller than race rest days due to the missing sq root
           RestDaysRaceMinbenefit = 39; %the minimium number of rest days from a race that a horse benefits from
           RestDaysRaceMaxbenefit = 60; %the maximium number of rest days from a race that a horse benefits from
           RestDaysWorkMinBenefit = 14; %the minimium number of rest days from a workout that a horse benefits from
           RestDaysWorkMaxBenefit = 70; %the minimium number of rest days from a workout that a horse benefits from
           
           
           
           %these three must always add up to 1!!!
           Baseline = .5;%this is the first tier that all that horses account for in the race, Acts like a pyramid as the horses that get closer to the horse get weighed more
           FirstTier = .4; % this applies to the closes four horses
           SecondTier = .1; % applies to the closest two horses
           
           
           
      end
      
      %these coeffs give ratio of out the gate relative comp variables:
      curroutgateexp = .4;
      prepoutgateexp = .4;
      ppoutgateexp = .3;
      
      %wrkout ratio to pp variables in the first relative competition
      %section
       wrkratiotopprelcomp = .6; 
      
      
       start = NumHorsesPerRaceTable((Table(horse,3)),5); %this is used for the current section for cleaning data
       stop =  NumHorsesPerRaceTable((Table(horse,3)),3);
       
       
       
       
       RaceDistance = Table(horse,6); %in YARDS
       
       %creates pp table to reference
       numpps = NumPPRacesPerHorse(horse,2);
       pp = cell(numpps,90); %pre-allocate table for each horse
       pptemp = Tablecell(horse,(256:1145)); %indexing the PP information located in Table
       for st = 0:(numpps-1) % counting the #of races so i can start at 0, its inclusive counting
           start = 1 + st; %makes start point move up by one every time for teach number of races
           colcount = 0; %prep to index pp
           rowcount = st + 1; %prep to index pp
           for lm = start:10:889 %goes by every ten since ten is what seperates each set of correlated data
               colcount = colcount + 1;
               pp(rowcount, colcount) = pptemp(1,lm); %reads every tenth number from column 256 to the end
           end
       end
       
       Brisparsconctemp = cell(1,numpps); %pre-allocating
       Brisparsconctemp(:) = Tablecell(horse, 1167:(1167+(numpps-1))); %getting ready to concatenate to column 90 of pp
       Brisparsconc = Brisparsconctemp';
       pp(:,90) = Brisparsconc; %puts brisnet pars in column 90
       
       [PProws, PPcols] = size(pp);
       
       CentralTemp = zeros(PProws, 15); %a temporary central matrix to put results, these will places into the Central table at the end of the loop
       
       PopulationTendancy = zeros(PProws,6); %pre-allocating
       raceplace = zeros(1,6); %pre-allocating
       
       %PP formulas/calculations go below
       if PProws == 0 %if horse has never raced before
           FINALppscore = 0;
           FINALworksscoreexp = FINALworksscoreexp + WrkWeightIfnoPPs; %makes the workscore weigh more if there is no pp history
           FINALpreperationscoreexp = FINALpreperationscoreexp + PrepweightIfnoPPs; %adds to prepscore
           FINALcurrentscoreexp = FINALcurrentscoreexp + CurrWeightIfnoPPs; %adds to current weight
           
           AvgPopTend = 1; %this is so that it doesnt change, might have to change
           PPcoef1temp = zeros(1,15); %pre-allocating
           
           recentraceweight = Table(horse,51); %if there is no recent weight data then the horse gets the current weigth carried, which will be calculated to 0
       elseif PProws > 0
           PPcoef1temp = zeros(PProws,15); %preallocating for the coef
           
           %pre-allocating
           ppweigthdecrease = linspace(1,Weightoflastraceinpp,numpps); %more recent races should be a more accurate predictor, so they should carry more weight
           
           recentraceweight = cell2mat(pp(1,26)); %for use in the current section
           for R = 1:PProws
               if isnan(cell2mat(pp(R,60))) % if any bris speeds are NaN
                   pp(R,60) = num2cell(min(cell2mat(pp(:,60)))); %sets the NaN to the minimium speed rating of all the pp races, this way a not fiinish doesnt harm too bad but still counts against technically since we give the lowest number
               end
           end
           
           BrisSpeedVariance = std((cell2mat(pp(:,90))) - (cell2mat(pp(:,60)))); %standard deviation for each horses bris speed compared to the winner, the lower this number the better
           if isnan(BrisSpeedVariance)
               BrisSpeedVariance = 10; %since lower is better giving a zero here would be unfair, so it seems like 10 is a common and higher number for this factor
           end
           
           
           for G = 1:PProws %PProws  %analyze the PP area
               
               
               restdaystemp1 = cell2mat(pp(G,2)); %remember that the last race in this will have NaN for rest days
               if isnan(restdaystemp1)
                   restdaystemp2 = RestDaysRaceMinbenefit; % puts the minimium benefit because that will count as the least benefit, and give horse benfit of the doubt
               else
                   restdaystemp2 = restdaystemp1;
               end
               
               
               if restdaystemp2 >= RestDaysRaceMinbenefit && restdaystemp2 <= RestDaysRaceMaxbenefit %if race rest days is in between beneficial range
                   restdays = (1/exp(1)) * (log10(restdaystemp2)); %this represents a benefit for more rest days within the range, if you check this graph wiht an x variable it shows a slow decrease in value for more rest days, which is what we want
               else
                   restdays = -(1/exp(1)) * (log10(restdaystemp2)); %negative version of above representing a decrease
               end
               
               racenum = cell2mat(pp(G,5)); %maybe can do something about time of day of race, a horse many be better in mornign vs. sitting at the track all day then racing
               
               
               %when i want incorporate the conditions, un comment the
               %respected sections.
               racecondition = string(cell2mat(pp(G,6))); %character array
               if racecondition == 'FT' %if pp race conditions are fast
                   raceconditionbonus = raceconditionbonusnum;
               else
                   raceconditionbonus = 1;
               end
               
               
               racedist = cell2mat(pp(G,7)); %absolute value becuase some nums are negative in code for about, i can ignore this
               %also in yards
               
               if racedist ~= RaceDistance %if that pp race is the same distance as todays race
                   CentralTemp(G,10) = 1;
               elseif racedist == RaceDistance
                   CentralTemp(G,10) = racedistboost + 1; %will make the score for the same distance weigh a little more
               end
               
               
               racesurface = cell2mat(pp(G,8)); %character
               if racesurface == RaceSurface %if pp race is on same surface
                   CentralTemp(G,8) = 1;
               elseif racesurface ~= RaceSurface
                  CentralTemp(G,8) = ppraceweightifnotsurface;
              end
              
                  
              racepopulation = cell2mat(pp(G,10)); % # of horses in race
              
              PopulationTendancy(G,1) = racepopulation; %this will let me knownwhat race population the horse performs and is most comfortable wiht
              
              
              racepostpositiontemp = cell2mat(pp(G,11));
              
              racepostposition = (1 / (abs((racepopulation / 2) - racepostpositiontemp) + 1) ); %middle is generally favored at the tracks.
              
              
              raceweight = cell2mat(pp(G,26)); %weight carried by horse in race
              CentralTemp(G,4) = raceweight;
              
              
             
              raceoddstemp = cell2mat(pp(G,27)); %odds for horse in race ?starting or finishing odds?
              if raceoddstemp == 0
                  raceodds = max(cell2mat(pp(:,27))); %gets max value for odds,because these odds haven been recorded
                    if isnan(max(cell2mat(pp(:,27))))
                        raceodds = 10; %if the max value race odds forthat horse is 0 we go with ten becuase ten is a general number SUBJECT TO CHNAGE
                    end
              else 
                  raceodds = raceoddstemp;
              end
              
              
              raceclass = char(pp(G,84)); %tells race class, for diffrent ones check dtate structure file
              
              if raceclass == 'M' %if horse has a maiden race
                  maidendrop = 1/2; %this will lower the race value factor because a maiden race has very weak horses
              else
                  maidendrop = 1;
              end
              
              MarginSeq = zeros(5,5); %preallocating to for margins
              
              racevalue = cell2mat(pp(G,31));
              
              race2ndmargin = cell2mat(pp(G,22)); %margin between the the 2nd horse and the winner
              MarginSeq(1,3) =  race2ndmargin;
              
              race3rdmarginbtwnleader = cell2mat(pp(G,23)) + race2ndmargin; %margin between 3rd finisher and the winner, added becuase i think that it is the right way but not 100%
              MarginSeq(1,4) = race3rdmarginbtwnleader;
              
              race4thmarginbtwnleader = cell2mat(pp(G,24)) + race3rdmarginbtwnleader; %margin between the forht place finisher and winner, added becase it is normally between the 3rd and 2nd horse but to get lenghts btwn leader you must add them
              MarginSeq(1,5) = race4thmarginbtwnleader;
              
              
              %if isnan the horse didnt finish, very bad but also could be a
              %bad indicator of a good horse
              raceplace = cell2mat(pp(G,32:37)); %horse's place over race at different calls in order from start to finish, maybe do a range calculation or something for this
              if isnan(raceplace(6))
                  PopulationTendancy(G,4) = racepopulation; %puts tnhe nans to the last place wich is the population
                  raceplacefinish = racepopulation; %oif no number recorded it came in last which is the population
              else
                  PopulationTendancy(G,4) = raceplace(6);
                  raceplacefinish = raceplace(6); 
              end
              raceldrmarginstarttemp = cell2mat(pp(G,39)); %margin between horse and leader, do if statement for if the horse is the leader, then its the margin betweeen him and second
              if isnan(raceldrmarginstarttemp) % if NaN then their tied so its .1 margin
                  raceldrmarginstart = mean(cell2mat(pp(:,39))); %since nan here means the number is unkown i just set it equal to the most common distance for starts for this horse
                  if isnan(mean(cell2mat(pp(:,39))))
                      raceldrmarginstart = 1; %instead of putting this as 0, which is tied for first, this puts the horse at a standard dsitance of 1 length, this means that the horse is 1 distance
                  end
              else
                  raceldrmarginstart = raceldrmarginstarttemp;
              end
              MarginSeq(5,1) = raceldrmarginstart;
             
              raceldrmargin1sttemp = cell2mat(pp(G,41)); %margin at 1st call
              if isnan(raceldrmargin1sttemp)
                  raceldrmargin1st = mean(cell2mat(pp(:,41))); %since nan here means the number is unkown i just set it to the median distance from leader for that race
                  if isnan(mean(cell2mat(pp(:,41))))
                      raceldrmargin1st = 1;
                  end
              else
                  raceldrmargin1st =raceldrmargin1sttemp;
              end
              MarginSeq(4,1) = raceldrmargin1st;
              
              
              raceldrmargin2ndtemp = cell2mat(pp(G,43)); %margin at 2nd call
              if isnan(raceldrmargin2ndtemp)
                  raceldrmargin2nd = mean(cell2mat(pp(:,43)));
                  if isnan(mean(cell2mat(pp(:,43))))
                      raceldrmargin2nd = 1;
                  end
              else
                  raceldrmargin2nd =raceldrmargin2ndtemp;
              end
              MarginSeq(3,1) = raceldrmargin2nd;
              
              
              raceldrmarginstretchtemp = cell2mat(pp(G,47)); %margin at the stretch
              if isnan(raceldrmarginstretchtemp)
                  raceldrmarginstretch = mean(cell2mat(pp(:,47)));
                  if isnan(mean(cell2mat(pp(:,47))))
                      raceldrmarginstretch = 1;
                  end
              else
                  raceldrmarginstretch = raceldrmarginstretchtemp;
              end
              MarginSeq(2,1) = raceldrmarginstretch;
              
              
              raceldrmarginfinishtemp = cell2mat(pp(G,49)); %margin btwn winner
              if isnan(raceldrmarginfinishtemp)
                  raceldrmarginfinish = mean(cell2mat(pp(:,49)));
                  if isnan(mean(cell2mat(pp(:,49))))
                      raceldrmarginfinish = 1;
                  end
              else
                  raceldrmarginfinish = raceldrmarginfinishtemp;
              end
              MarginSeq(1,1) = raceldrmarginfinish;
              
              
              for x = 4:-1:1 %this goes backwards from 2nd last row to top
                  MarginSeq(x,2) = MarginSeq(x,1) - MarginSeq(x+1,1); %subtracts predicted value form how it performed
                  
              end
              
              NormalizedLateMarginSpeed = (MarginSeq(1,1) - MarginSeq(1,3)) - MarginSeq(2,1); %this "normalizes the distance figures, this avoids giving a bad race rating for a horse where the winner was killing th ecompetition, that wouldnt faily represent this horses ability,and so you subtract the 2nd place finisher from the first place.
              
              PopulationTendancy(G,2) = NormalizedLateMarginSpeed;
              
              EarlyMarginSpeed = MarginSeq(4,1) - MarginSeq(5,1); %gets distance lost or gained between horse and leader early on in the race
              
              MidMarginSpeed = MarginSeq(2,1) - MarginSeq(3,1); %more negative the better
            
              LateMarginSpeed = MarginSeq(1,1) - MarginSeq(2,1); %more negative the better
              
              MarginTrend = (sum(MarginSeq(:,2))/4);%trend  over race for distance between ldr, from start to finish, divided by four becuase that is number of margins
                           
              PopulationTendancy(G,3) = MarginTrend;
              
              
              PopTend = PopulationTendancy(G,1) / PopulationTendancy(G,4); %gets a ratio for the finishing place in race per the race population, the higher number the better
              PopulationTendancy(G,5) = PopTend;
              
              
              Bris2fPace = cell2mat(pp(G,52)); %2nd furlong call pace figure
              Bris4fPace = cell2mat(pp(G,53)); % 4th furlong pace
              Bris6fPace = cell2mat(pp(G,54)); %EXISTS ONLY IF IDSTANCE is > 6 furlongs
              Bris8fPace = cell2mat(pp(G,55)); %ONLY IF Race is 8 ufrlongs
              BrisfinalPace = cell2mat(pp(G,57)); 
              
             
              
              %if bris is isNAN the horse didnt finish
              if isnan(cell2mat(pp(G,60)))
                  bristemp = cell2mat(pp(:,60));
                  Brisspeedrating = min(bristemp); %if the horse didnt finish then take the minimium bris speed ratign from other races
                  if isnan(bristemp)
                      Brisspeedrating = 50; %putting a fifty for a horse who is missing all his speed ratings, dont know what else
                  end
              else
                  Brisspeedrating = cell2mat(pp(G,60));
              end
              CentralTemp(G,6) = Brisspeedrating;
              
              
              BrisSpeedPar = cell2mat(pp(G,90));  %the average speed of the winner for todays race 
              
              Speedrating = cell2mat(pp(G,61)); %not calculate  by brisnet, looks less accurate honestly
              
              Trackvariant = cell2mat(pp(G,62));
              
              
             
              
              
              NormalizedBrisSpeedtemp = BrisSpeedPar - Brisspeedrating; %subtracts the needed bris speed to win from the bris speed achieved by the horse
              if NormalizedBrisSpeedtemp <= 0 %if negative, means that horse was the leader or ran above the par, so it would be posiitve
                  NormalizedBrisSpeed = (sqrt(abs(NormalizedBrisSpeedtemp)));
              elseif NormalizedBrisSpeedtemp > 0
                   NormalizedBrisSpeed = -(sqrt(NormalizedBrisSpeedtemp));
              end
              CentralTemp(G,11) = NormalizedBrisSpeed;
              
              if CentralTemp(G,5) <= 0 %if horse went down in weight
                  raceweightcarried = 1; %MAYBE GET A CALCULATION HERE
              elseif CentralTemp(G,5) > 0 %if horse went up in weight
                  raceweightcarried = 1.2;
              end
              
              PastCalculations = restdays*restdaysexp + sqrt(raceodds^-1)*(1/(sqrt(raceplacefinish)))*raceoddsexp + ... %sqrt inverse becuase lower numbers are better but the highest numbers shouldnt peanelize thatbad check graph sqrt(x^-1)
                  log(racevalue*racevalueexp*maidendrop+1) + (NormalizedBrisSpeed)*NormalizedBrisSpeedexp +  ...
                  sqrt(racepostposition)*racepostpositionexp + ...
                  raceweightcarried*raceweightcarriedexp + ...
                  -atan(EarlyMarginSpeed)*EarlyMarginSpeedexp*startadvantagetable(horse,1) + -atan(MidMarginSpeed)* MidMarginSpeedexp + ...
                  -atan(LateMarginSpeed)*LateMarginSpeedexp + -atan(MarginTrend)*MarginTrendexp + ...
                  -atan(NormalizedLateMarginSpeed)*NormalizedLateMarginSpeedexp;
              
              CentralTemp(G,1) = PastCalculations * raceconditionbonus; %multiplies by the race condition bonus if the conditions were similar to fast and dry, which is the only consitions i am betting on
                  
              
              PPcoef1temp(G,1) = restdays;
              PPcoef1temp(G,2) = sqrt(raceodds^-1);
              PPcoef1temp(G,3) = log(racevalue+1);
              PPcoef1temp(G,4) = (NormalizedBrisSpeed);
              PPcoef1temp(G,5) = raceweightcarried;
              PPcoef1temp(G,6) = -atan(EarlyMarginSpeed);
              PPcoef1temp(G,7) = -atan(MidMarginSpeed);
              PPcoef1temp(G,8) = -atan(LateMarginSpeed);
              PPcoef1temp(G,9) = -atan(MarginTrend);
              PPcoef1temp(G,10) =  -atan(NormalizedLateMarginSpeed);
            
              
              
          end
          
          
%           %BRIS SPEED CALCULATIONS
%           for L = PProws:-1:1 %goes backwards from bottom to top
%               Avgtemppredicton = mean(CentralTemp(L:end,6)); %gets the average for the scores up to the past race im doing, and nothing past that so its a true prediction
%               
%               PredictedBrisSpeed = BrisSpeedVariance*CentralTemp(L,2) + Avgtemppredicton ; %multiplies the total vairance  by the outperformance factor based on our prediction methods, and adds it to the average of the bris speeds of the races before it, this then gives what bris speed we think the horse will get in the next race
%               CentralTemp(L,8) = PredictedBrisSpeed;
%               BrisNewVariance = BrisSpeedVariance*CentralTemp(L,2);
%               CentralTemp(L,7) = CentralTemp(L,6) +  BrisNewVariance; %creates upper bound for bris scores by adding the given score to the variance multiplied by outperformance/underperformance factor
%           end
%           for x = PProws-1:-1:1 %this goes backwards from 2nd last row to top
%               CentralTemp(x,9) = CentralTemp(x,7) - CentralTemp(x,8); %this subtracts the actual ran bris speed based upon our calcualtions(which modify thebris speed given too it), then it subtracts what we have predicted, based upon past races from that race, gets that difference
%           end
%                 
%           AverageBrisSpeedOutperformance = mean(CentralTemp(:,9)); %gets the average of the predicted bris value difference from the actual result

         
          

          
          %this loop gets the difference between the predicted odds and
          %how the horse scored
          %also if the horse is on a constant decline in performance,
          %it is bad and will take into account
          for x = PProws-1:-1:1 %this goes backwards from 2nd last row to top
              AverageScoresBefore = mean(CentralTemp(x+1:end,1)); %gives the average of all that scores that came before the race that im predicting
              CentralTemp(x,2) = CentralTemp(x,1) - AverageScoresBefore; %subtracts average of scores before the race from thescore it got in the race
              CentralTemp(x,5) = CentralTemp(x,4) - CentralTemp(x+1,4); %gets whether the horse went up or down in weight carried
              OutperformanceTrend = CentralTemp(x,2) - CentralTemp(x+1,2); %gets the difference between the last outperfromance and the next one to get trend, like deravitive
              CentralTemp(x,13)  = OutperformanceTrend;
          end
          
           for F = 1:PProws
               %               CentralTemp(F,10) = CentralTemp(F,1) + (CentralTemp(F,7) + CentralTemp(F,9)) * Brisspeedratingexp; %takes the score calcualted above, then adds the bris speed ratings +/- wheter it outperformed or underperformed based upon my predictions, then multiplied for its weight in the final score
               CentralTemp(F,3) = ((CentralTemp(F,1))*CentralTemp(F,8) * CentralTemp(F,10))*(ppweigthdecrease(F)); %multiplies each factor based upon the weight each race decreasing over time as we go back in racces, as well as the weight of each past race if not on same surface
               CentralTemp(F,14) = (CentralTemp(F,13)*(ppweigthdecrease(F))); %makes the most relevant trend is outperformance the most relevant and applicable
               PopulationTendancy(F,6) = (PopulationTendancy(F,5)*(ppweigthdecrease(F)));
           end
           
           AvgPopTend = (sqrt(sum(PopulationTendancy(:,6)))) / PProws; %this number represents the average race finishing place pere each opposing horse in the race, this is sqrted becuase outliers will be less effcetive, whilst contributing still, ppweightdecrease is calculated in here becuase as ahorse gets more experiecne i believ that they become more comfortable with racing in large numbers, so the closer numbers are more representative
           
           AverageOutperformance = mean(CentralTemp(:,2)); %the difference between the predited value based upon PastCalculations and how it perfromed based on PastCalculations
           AverageOutperformanceTrend = mean(CentralTemp(:,14)); %gets the average trend witht the most recent trends weighiign the most due to the ppweight decrease factor being weighed in in the loop above
           
           FINALppscore = mean(CentralTemp(:,3)) +  (mean(CentralTemp(:,3))*(AverageOutperformanceTrend*AverageOutperformanceexp));
              %what im thinkign with this equation is that multiplying the
              %outperfromance trned which tells based upon our predicition
              %how accruate, relevant to the past races, the horse has
              %scored, and we can assume that the horse will score again
              %next based upon the more recent trends in perfromance,again
              %relative to our scoring methods. This will give the horse
              %his score, then add or subtract a portion of that same score
              %based upon wheter the horse has been out or under performing
              
              
              
              
              if FINALppscore <= 0 %if final score is negative
                 Central(horse,6) = 0;%-(sqrt(abs(FINALppscore)))*FINALppscoreexp; %negate  the absolute value of the score
              %just gigving horse a 0 most likely becuase they dont have
              %past races, this way it wont count against
              elseif FINALppscore > 0 %if final score is positive
                 Central(horse,6) = sqrt(FINALppscore)*FINALppscoreexp;
              end
              
              PPcoef1(horse,1) = AverageOutperformanceTrend;
              PPcoef1(horse,2) = mean(PPcoef1temp(:,1)); %average of rest days
              PPcoef1(horse,3) = mean(PPcoef1temp(:,2));
              PPcoef1(horse,4) = mean(PPcoef1temp(:,3));
              PPcoef1(horse,5) = mean(PPcoef1temp(:,4));
              PPcoef1(horse,6) = mean(PPcoef1temp(:,5));
              PPcoef1(horse,7) = mean(PPcoef1temp(:,6));
              PPcoef1(horse,8) = mean(PPcoef1temp(:,7));
              PPcoef1(horse,9) = mean(PPcoef1temp(:,8));
              PPcoef1(horse,10) = mean(PPcoef1temp(:,9));
              PPcoef1(horse,11) = mean(PPcoef1temp(:,10));
              
              
      end
      
      
      
      
      numworks = NumPPRacesPerHorse(horse,5); %creates workout table to reference
      works = cell(numworks,9); %pre-allocate table for each horse
      worktemp = Tablecell(horse,(102:209));%indexing the PP information located in Table
      for wst = 0:(numworks-1) % counting the #of races so i can start at 0, its inclusive counting
          wstart = 1 + wst; %makes start point move up by one every time for teach number of races
          wcolcount = 0; %prep to index pp
          wrowcount = wst + 1; %prep to index pp
          for wlm = wstart:12:108 %goes by every ten since ten is what seperates each set of correlated data
              wcolcount = wcolcount + 1;
              works(wrowcount, wcolcount) = worktemp(1,wlm); %reads every tenth number from column 256 to the end
          end
      end
      
      [worksrow, workscol] = size(works);
      
      worksCentral = zeros(worksrow,8); %preallocating for workout matrix
      
      %workout formula/calculations goes in this for loop below
      
      for wrkanl = 1:worksrow %analyze for each workout
          
          WorksCoef1temp = zeros(worksrow,2); %pre-allocating
          
          workdatetemp1 = string(works(wrkanl,1));
          workdatetemp = char(workdatetemp1);
          workYtemp = workdatetemp(1:4);
          workMtemp = workdatetemp(5:6);
          workDtemp = workdatetemp(7:8);
          WorkYear = str2double(workYtemp);
          WorkMonth = str2double(workMtemp);
          WorkDay = str2double(workDtemp);
          wdoytemp = datetime(WorkYear,WorkMonth,WorkDay);
          WorkDayofyear = day(wdoytemp,'dayofyear');
          
          
          worksCentral(wrkanl,1) = WorkYear; %puts into central so i can analyze resdays
          worksCentral(wrkanl,2) = WorkDay;
          worksCentral(wrkanl,3) = WorkMonth;
          worksCentral(wrkanl,4) = WorkDayofyear;
          
          workdist = (cell2mat( works(wrkanl,4)) / 220); %divide by 22o becuase yards/220 = furlongs, and i need a linear representation in smaller numbers
          
          workconditions = cell2mat(works(wrkanl,5)); %character
          
          numworksperday = sqrt(cell2mat(works(wrkanl,8))); %number of workouts the horse performed that day, sqrt because i think that getting at lower number of wrks per day in more than an excessive number, which would be throwing off the prediction, sqrt graph tepresents this
          %consistency in workout is very important, do range for rest days
          %the lower range equals more consistency and the over all lower
          %rest days num equals more workouts, which is better
          
          
          
          Workscalculations = workdist*workdistexp + numworksperday*numworksperdayexp;
          
          worksCentral(wrkanl,6) = Workscalculations;
          
          
          WorksCoef1temp(wrkanl,1) = workdist;
          WorksCoef1temp(wrkanl,2) = numworksperday;
          
      end
      
      RecentWorkDayofYear = worksCentral(1,4); %most recent workout, used for calculating work rest days
      RecentWorkYear = worksCentral(1,1); %gives morst recent workout year
      
      
      %this calculates the number of rest days between workouts and puts it
      %into the workcentral matrix in column 5
      for restwork = 2:worksrow
          if worksCentral(restwork-1,1) == worksCentral(restwork,1) %if years are equal
              workrestdays = worksCentral(restwork-1,4) - worksCentral(restwork,4);
          elseif worksCentral(restwork-1,1) ~= worksCentral(restwork,1) %if years aren't equal
              firstDayInYearNum = datenum(worksCentral(restwork,1),1,1);
              lastDayInYearNum = datenum(worksCentral(restwork,1),12,31);
              numDaysInYear = lastDayInYearNum - firstDayInYearNum + 1;
              workresttemp = numDaysInYear - worksCentral(restwork,4);
              workrestdays = workresttemp + worksCentral(restwork-1,4);
          end
          worksCentral(restwork-1,5) = workrestdays;
      end
      
      
      
      worksconsistencyaverage = std(worksCentral(1:end-1,5)); %use standard deviation to last row minus one becuase last row will always be 0 
      
      worksavg = mean(worksCentral(:,6));
      %REFINE, maybe add exponents to this
      FINALworksscore = exp(-worksconsistencyaverage)*worksconsistencyaverageexp + sqrt(worksavg)*worksavgexp; %final score for this horses workouts, 1/consistency makes it so that that a larger range means less consistency
      %works consistency average is a negative exponential, becuase if you
      %look at the graph for e^-x it a lower number represents a larger
      %value, and that means that when the horse is more consistent (lower
      %tsandard deviation) it will benefit the horse
      
      Workcoef1(horse,1) = mean(WorksCoef1temp(:,1));
      Workcoef1(horse,2) = mean(WorksCoef1temp(:,2));
      Workcoef1(horse,3) = exp(-worksconsistencyaverage);
      Workcoef1(horse,4) = sqrt(worksavg);
     
     
     
     
      %puts work score in central matrix column 5
      if FINALworksscore <= 0 %if final score is negative
           Central(horse,5) = -(sqrt(abs(FINALworksscore)))*FINALworksscoreexp; %negate the absolute value square root, becuase cannot take sqrt of negative
      elseif FINALworksscore > 0
           Central(horse,5) = sqrt(FINALworksscore)*FINALworksscoreexp;
      end
      
      
     
      
      
      
      PostPositiontemp = Table(horse,4); %the horses' post position
      RaceNumtemp = Table(horse,3); %what race each horse is in
      RacePopulation = NumHorsesPerRaceTable(RaceNumtemp,2); %determines the number of horses in the race
      TrackBias = 2; %THIS is where i put the information for post position track bias, can be 1, 2, or 3
      
      Populationfinishprediciton = RacePopulation*AvgPopTend;
      Central(horse,30) = Populationfinishprediciton; % the lower the number here relative to the other horses in this race is better
      
      
      if TrackBias == 1 %if inside is favored
          PostPosition = (RacePopulation + 1) - PostPositiontemp;
      elseif TrackBias == 2 %if middle is favored
          PostPosition = (1 / (abs((RacePopulation / 2) - PostPositiontemp) + 1) );
      elseif TrackBias == 3 %if ouside is favored
          PostPosition = PostPositiontemp - (RacePopulation + 1);
      end
 
      
      RacePurse = Table(horse,12); %if its claiming use column 13
      
      RaceClaimingPrice = Table(horse,13); %value of race if its claiming
      
      ClaimingPriceHorse = Table(horse,14); %claiming price of horse
      
      if Table(horse,29) ~= 0 %if trainer has at least 1 race experience
          Trainer1winpercentage = (Table(horse,30)/Table(horse,29)); % 1st place win percentage
          Trainer2winpercentage = (Table(horse,31)/Table(horse,29)); % 2nd place win percentage
          Trainer3winpercentage = (Table(horse,32)/Table(horse,29)); % 3rd place win percentage
          Trainertop3winpercentage = Trainer1winpercentage + Trainer2winpercentage + Trainer3winpercentage; %win % for top 3 finish
      elseif Table(horse,29) == 0 %if trainer has no expeience
          Trainer1winpercentage = 0;
          Trainer2winpercentage = 0;
          Trainer3winpercentage = 0;
          Trainertop3winpercentage = 0;
      end
      
      if Table(horse,1157) ~= 0 %this is for Jockeys current year, better representation of their abilities than life, athletes perspective
          Jockey1winpercentage = (Table(horse,1158)/Table(horse,1157));
          Jockey2winpercentage = (Table(horse,1159)/Table(horse,1157));
          Jockey3winpercentage = (Table(horse,1160)/Table(horse,1157));
          Jockeytop3winpercentage = Jockey1winpercentage + Jockey2winpercentage + Jockey3winpercentage;
      elseif Table(horse,1157) == 0 %if jockey has no expeience
          Jockey1winpercentage = 0;
          Jockey2winpercentage = 0;
          Jockey3winpercentage = 0;
          Jockeytop3winpercentage = 0;
      end
      
      
      MorningOdds = Table(horse,44); %this is just the raw num, so larger is worse, check this
      
      
      HorseBirthYear = 2000 + Table(horse,46);
      HorseBirthMonth = Table(horse,47);
      HorseAgeinMonthstemp = between(datetime(HorseBirthYear,HorseBirthMonth, 15),(datetime('now')),{('Months')}); %Months old, assuming each horse born mid month becuase that information isnt provided
      HorseAgeinMonths = split(HorseAgeinMonthstemp, {'months'}); %converts calender duration to numeric
      
      
      HorseWeightcarriedtemp = Table(horse,51); %i think this is weight carried, increase in weight over last race increases chances of winning statistically 
      Weighttemp = HorseWeightcarriedtemp - recentraceweight;
      if Weighttemp <= 0 %if horse went down in weight
          HorseWeigthCarried = sqrt(abs(Weighttemp)); %counts for
      elseif Weighttemp > 0 %if horse went up in weight
           HorseWeigthCarried = -(sqrt(abs(Weighttemp)));% counts against
      end
      
      
      HorseBlinkertemp = Table(horse,64); %whether the horse has blinkers or not
      if HorseBlinkertemp == 1
          HorseBlinkerstatus = 2; %will be taking -log of this
      else
          HorseBlinkerstatus = 1; %doesnt count against for no blinkers
      end
      
      
      if Table(horse,65) ~= 0
          HorseAtDist1stpercentage = Table(horse,66)/Table(horse,65); %races at this distance
          HorseAtDist2ndpercentage = Table(horse,67)/Table(horse,65);
          HorseAtDist3rdpercentage = Table(horse,68)/Table(horse,65);
          HorseAtDistTop3Percentage = HorseAtDist1stpercentage + HorseAtDist2ndpercentage + HorseAtDist3rdpercentage;
      elseif Table(horse,65) == 0 
          HorseAtDist1stpercentage = 0;
          HorseAtDist2ndpercentage = 0;
          HorseAtDist3rdpercentage = 0;
          HorseAtDistTop3Percentage = 0;
      end
      if Table(horse,69) == 0 %if horse has no earnings
          HorseEarnAtDist = 0; %set equal to 0 
      else
          HorseEarnAtDist = Table(horse,69); %earnings at this distance
      end
      
      
      
      if Table(horse,70) ~= 0
          HorseAtTrack1stpercentage = Table(horse,71)/Table(horse,70); %races at this distance
          HorseAtTrack2ndpercentage = Table(horse,72)/Table(horse,70);
          HorseAtTrack3rdpercentage = Table(horse,73)/Table(horse,70);
          HorseAtTrackTop3Percentage = HorseAtTrack1stpercentage + HorseAtTrack2ndpercentage + HorseAtTrack3rdpercentage;
      elseif Table(horse,70) == 0
          HorseAtTrack1stpercentage = 0;
          HorseAtTrack2ndpercentage = 0;
          HorseAtTrack3rdpercentage = 0;
          HorseAtTrackTop3Percentage = 0;
      end
      if Table(horse,74) == 0
          HorseEarningsAtTrack = 0; 
      else
          HorseEarningsAtTrack = Table(horse,74); %earnings at todays track, gets logged in calculatins
      end
      
      
      
      if Table(horse,75) ~= 0 %if horse has turf experience
          HorseLifeTurf1stPercentage = Table(horse,76)/Table(horse,75); %percentage win on turf
          HorseLifeTurf2ndPercentage = Table(horse,77)/Table(horse,75);
          HorseLifeTurf3rdPercentage = Table(horse,78)/Table(horse,75);
          HorseLifeTurfTop3Percentage =  HorseLifeTurf1stPercentage +  HorseLifeTurf2ndPercentage +  HorseLifeTurf3rdPercentage;
      elseif Table(horse,75) == 0
          HorseLifeTurf1stPercentage = 0;
          HorseLifeTurf2ndPercentage = 0;
          HorseLifeTurf3rdPercentage = 0;
          HorseLifeTurfTop3Percentage = 0;
      end
      if Table(horse,79) == 0
          HorseLifeTurfEarnings = 0;
      else
          HorseLifeTurfEarnings = Table(horse,79);
      end
      
      
      if Table(horse,80) ~= 0
          HorseLifeWet1stPercetage = Table(horse,81)/Table(horse,80); %percentage in wet conditions
          HorseLifeWet2ndPercetage = Table(horse,82)/Table(horse,80);
          HorseLifeWet3rdPercetage = Table(horse,83)/Table(horse,80);
          HorseLifeWetTop3Percetage = HorseLifeWet1stPercetage + HorseLifeWet2ndPercetage + HorseLifeWet3rdPercetage;
      elseif Table(horse,80) == 0
          HorseLifeWet1stPercetage = 0;
          HorseLifeWet2ndPercetage = 0;
          HorseLifeWet3rdPercetage = 0;
          HorseLifeWetTop3Percetage = 0;
      end
      if Table(horse,84) == 0
          HorseLifeWetEarnings = 0;
      else
          HorseLifeWetEarnings = Table(horse,84);
      end
      
      
      if Table(horse,1332) ~= 0
          HorseLifeFastDirt1stPercentage = Table(horse,1333)/Table(horse,1332); %percentage 1st place win on fast dirt
          HorseLifeFastDirt2ndPercentage = Table(horse,1334)/Table(horse,1332);
          HorseLifeFastDirt3rdPercentage = Table(horse,1335)/Table(horse,1332);
          HorseLifeFastDirtTop3Percentage = HorseLifeFastDirt1stPercentage + HorseLifeFastDirt2ndPercentage + HorseLifeFastDirt3rdPercentage;
      elseif Table(horse,1332) == 0
          HorseLifeFastDirt1stPercentage = 0;
          HorseLifeFastDirt2ndPercentage = 0;
          HorseLifeFastDirt3rdPercentage = 0;
          HorseLifeFastDirtTop3Percentage = 0;
      end
      if Table(horse,1336) == 0
          HorseLifeFastDirtEarnings =  0;
      else
          HorseLifeFastDirtEarnings = Table(horse,1336);
      end
      
      
      
      if Table(horse,97) ~= 0
          HorseLife1stRecord = Table(horse,98)/Table(horse,97); %percentage win 1st place lifetime
          HorseLife2ndRecord = Table(horse,99)/Table(horse,97);
          HorseLife3rdRecord = Table(horse,100)/Table(horse,97);
          HorseLifeTop3Record = HorseLife1stRecord + HorseLife2ndRecord + HorseLife3rdRecord;
      elseif Table(horse,97) == 0
          HorseLife1stRecord = 0;
          HorseLife2ndRecord = 0;
          HorseLife3rdRecord = 0;
          HorseLifeTop3Record = 0;
      end
      if Table(horse,101) == 0
          HorseLifeEarnings = 0;
      else
          HorseLifeEarnings = Table(horse,101);
      end
      
      
      if Table(horse,86) ~= 0
          HorseCurYear1stRecord = Table(horse,87)/Table(horse,86); %percentage win 1st place in the current year
          HorseCurYear2ndRecord = Table(horse,88)/Table(horse,86);
          HorseCurYear3rdRecord = Table(horse,89)/Table(horse,86);
          HorseCurYearTop3Record = HorseLife1stRecord + HorseLife2ndRecord + HorseLife3rdRecord;
      elseif Table(horse,86) == 0
          HorseCurYear1stRecord = 0;
          HorseCurYear2ndRecord = 0;
          HorseCurYear3rdRecord = 0;
          HorseCurYearTop3Record = 0;
      end
      if Table(horse,90) == 0
          HorseCurYearEarnings = 0;
      else
          HorseCurYearEarnings = Table(horse,90);
      end
      
      
      %if isnan horse has never raced before
      if isnan(Table(horse,224))
          RaceRestDaystemp = RestDaysRaceMinbenefit; %giving benefit of the doubt becuase horse has low chance
      else
          RaceRestDaystemp = Table(horse,224); %the number of days between current race and last race
      end
      
      %Calculates rest days between the last workout and todays race
      if RY == RecentWorkYear %if the last wrokout was the same year as race year
      WorkRestDays = RaceDayofYear - RecentWorkDayofYear; %gets the rest days in between the last race for each horse
      elseif RY ~= RecentWorkYear %if the years arent the same, calculate time between
        firstDayInYearNum = datenum(RecentWorkYear,1,1);
        lastDayInYearNum = datenum(RecentWorkYear,12,31);
        numDaysInYear = lastDayInYearNum - firstDayInYearNum + 1;
        workrestyeardiff = numDaysInYear - RecentWorkDayofYear; %gets days till end of year for workout
        WorkRestDays = RaceDayofYear + workrestyeardiff;
      end
      
      %FOR REST DAYS
      %Rest is good for a horse within a certian range but more than that race
      %becomes bad
      if RaceRestDaystemp >= RestDaysRaceMinbenefit && RaceRestDaystemp <= RestDaysRaceMaxbenefit %if race rest days is in between beneficial range
          RaceRestDays = (1/exp(1)) * (log10(RaceRestDaystemp)); %this represents a benefit for more rest days within the range, if you check this graph wiht an x variable it shows a slow decrease in value for more rest days, which is what we want
      else
          RaceRestDays = -(1/exp(1)) * (log10(RaceRestDaystemp)); %negative version of above representing a decrease
      end
      
      if WorkRestDays >= RestDaysWorkMinBenefit && WorkRestDays <= RestDaysWorkMaxBenefit
          WorkRestDays =  (1/exp(1)) * log10((WorkRestDays)); %since a small number in this range is better
      else
          WorkRestDays =  -(1/exp(1)) * log10((WorkRestDays));
      end
      

      
      if isnan(Table(horse,1178)) || Table(horse,1178) == 0 %if best speed is nan set to 0
          BestSpeedFastDirt = std(Table(:,1178)) + 10; %takes standard deviation of all horses racing and adds, to get a low number, but better than 0 for sure 
      else
          BestSpeedFastDirt = Table(horse,1178); %best brisnet speed figure for fast tracks, use this with pp to create a range of potential for each horse
      end
      
      if isnan(Table(horse,1179)) || Table(horse,1179) == 0 %if best speed is nan set to 0
          BestSpeedTurf = std(Table(:,1179)) + 10;
      else
          BestSpeedTurf = Table(horse,1179);
      end
      
      if isnan(Table(horse,1181)) || Table(horse,1181) == 0 %if best speed is nan set to 0
          BestSpeedDistance = std(Table(:,1181)) + 10;
      else
          BestSpeedDistance = Table(horse,1181);
      end
      
      
      %if statement for determining which factors to use for what surfaces
      
      if RaceSurface == 'D'
          Brisspeedonsurface = BestSpeedFastDirt;
          HorseLifeTop3winsurface = HorseLifeFastDirtTop3Percentage;
          HorseLife1stwinsurface = HorseLifeFastDirt1stPercentage;
          
      elseif RaceSurface == 'T'
          Brisspeedonsurface = BestSpeedTurf;
          HorseLifeTop3winsurface = HorseLifeTurfTop3Percentage;
          HorseLife1stwinsurface = HorseLifeTurf1stPercentage;
          
      end
      
      
      %going to have to look up weather/track conditions day of then enter
      %manually into the program, probably at top of script.
      %       if RaceConditions = wet
      
      
      
      %calcuates final score based upon the data up to the current day
      
      CurrentCoef1(horse,1) = ((1/MorningOdds)^2);
      CurrentCoef1(horse,2) = (log(Brisspeedonsurface+1));
      CurrentCoef1(horse,3) = ((HorseLifeTop3winsurface^2));
      CurrentCoef1(horse,4) = ((HorseLife1stwinsurface^2));
      CurrentCoef1(horse,5) = ((HorseCurYearTop3Record^2));
      CurrentCoef1(horse,6) = ((HorseAtDistTop3Percentage^2));
      CurrentCoef1(horse,7) = ((HorseAtTrackTop3Percentage^2));
      CurrentCoef1(horse,8) = ((HorseAtDist1stpercentage^2));
      CurrentCoef1(horse,9) = sqrt(1/Populationfinishprediciton);
      
      
      FINALcurrentscore = ...
          MorningOddsexp*((1/MorningOdds)^2) + ...
          log(Brisspeedonsurfaceexp*Brisspeedonsurface+1) + HorseLifeTop3winsurfaceexp*(HorseLifeTop3winsurface) + ...
          (HorseLife1stwinsurface)*HorseLife1stwinsurfaceexp + (HorseCurYearTop3Record)*HorseCurYearTop3Recordexp + ...
          (HorseAtDistTop3Percentage)*HorseAtDistTop3Percentageexp + (HorseAtTrackTop3Percentage)*HorseAtTrackTop3Percentageexp + ...
          (HorseAtDist1stpercentage)*HorseAtDist1stpercentageexp + sqrt(1/Populationfinishprediciton)*Populationfinishpredicitonexp;
      
      if FINALcurrentscore <= 0 
         Central(horse,4) = -(sqrt(abs(FINALcurrentscore)))*FINALcurrentscoreexp;
      elseif FINALcurrentscore > 0 
          Central(horse,4) = sqrt(FINALcurrentscore)*FINALcurrentscoreexp;
      end
      
      PrepCoef1(horse,1) = log(HorseAgeinMonths);
      PrepCoef1(horse,2) = HorseWeigthCarried;
      PrepCoef1(horse,3) = RaceRestDays;
      PrepCoef1(horse,4) = Trainertop3winpercentage;
      PrepCoef1(horse,5) = Jockeytop3winpercentage;
      PrepCoef1(horse,6) = PostPosition;
      PrepCoef1(horse,7) = -log(HorseBlinkerstatus);
      PrepCoef1(horse,8) =  WorkRestDays;
      
     
      FINALpreperationscore = log(HorseAgeinMonths)*HorseAgeinMonthsexp + HorseWeigthCarried*HorseWeigthCarriedexp + ...
          RaceRestDaysexp*RaceRestDays + Trainertop3winexp*Trainertop3winpercentage + Jockeytop3winexp*Jockeytop3winpercentage + ...
          PostPosition*PostPositionexp + -log(HorseBlinkerstatus)*HorseBlinkerstatusexp + WorkRestDaysexp*WorkRestDays;
      
      
      
      
      if FINALpreperationscore <= 0
          Central(horse,10) = -(sqrt(abs(FINALpreperationscore)))*FINALpreperationscoreexp;
      elseif FINALpreperationscore > 0
          Central(horse,10) = sqrt(FINALpreperationscore)*FINALpreperationscoreexp;
      end
      
      
  
      
      Totalscoresum = Central(horse,6) + Central(horse,5)*wrkratiotopprelcomp; % i took out prep and current becuase of the relative comp looppart two
      
      Central(horse,7) = Totalscoresum;
      
      Outthegatecomp = Central(horse,4)*curroutgateexp + Central(horse,10)*prepoutgateexp + Central(horse,6)*ppoutgateexp; %out the gate this means that a horse's performance out the gate jump is more reliant upon the prep and current sections
      %might want to add into this a horses past performances out the gate
      %start
      Central(horse,31) = Outthegatecomp; 
     
      Laststretch = Central(horse,6); %this last stretch of the race willl rely solely on the horses past exprerince
      
       Central(horse,33) = Laststretch;
  end
  
  
  fprintf('%s - %i/%i/%i\n', TrackName, RM, RD, RY) %prints what track was just run, and what results are for
  %morning line odds are different from public odds, find out how to incorporate into formula and where')
  
  %Scratches loop
  for s = 1:NumRaces %goes through each race
      for t = 1:Rows %goes through each horse, although not this many hroses in eac race, but will still work this way
          if Scratches(s,t) == 1 %if horse is scratches according to my input
               
              
              HorseNumTemp1 = NumHorsesPerRaceTable(s,5); %gets first horse in each race
              HorseNumTemp2 = HorseNumTemp1 + (t-1); %adds the number for the horse in the race, but subtracts one becuase it inclusive counting
              Central(HorseNumTemp2,:) = []; %deletes entire row for that horse, before relative competition and sorting is run
              
              for p = s:NumRaces %this redefines the start and end positions for the numhorses table thing
                  NumHorsesPerRaceTable(p,3) = NumHorsesPerRaceTable(p,3) - 1; %subtracts one from this race until the end race
                  NumHorsesPerRaceTable(p,4) = NumHorsesPerRaceTable(p,4) - 1;
                  NumHorsesPerRaceTable(p,5) = NumHorsesPerRaceTable(p,5) - 1; 
              end
              
          end
      end
  end
  
  %this correctly redifines the fifth column
  for k = 1:NumRaces
  NumHorsesPerRaceTable(k,5) = (NumHorsesPerRaceTable(k,3) - (NumHorsesPerRaceTable(k,2) - 1)); %thi redifines the number of horses in each race

  
  end
  
 NumHorsesPerRaceTable(1,4) = 1;
 NumHorsesPerRaceTable(1,5) = 1;
  
   
  
  [Rows,~] = size(Central); %redifines the number of horses in the table, so that following calculations make sense
  
  
  

 %RELATIVE COMPETITION - pyramid style  \ Out the gate loop, uses Central
 %column 31
 
   %this loop goes through and compares each horse to the horse next to it
   %as well as weighs the horses next to it by a larger factor, since they
   %will be competing more directly with that horse
  RaceSurfaceTable = cell(NumRaces,1); %pre-allocating
   
   for r = 1:NumRaces
       
       start = NumHorsesPerRaceTable(r,5);
       stop =  NumHorsesPerRaceTable(r,3);
       raceTotal = sum(Central(start:stop,31)); %gets sum of scores for each race
       RaceSurfaceTable(r,1) = Tablecell(start,31); %this doesnt affect this loop
       %Each total also fully takes into account a horses own score, this may be wrong but you have to remember this si still about taking averages, so i need to fully weigh each horses own, to get fair representation
       %these horses are compared to the closest four horses, 5 including
       %themselves, the closest horse compettition factor takes every horse
       %in the race at that factor,then closest factor minus one takes 1
       %minus closest horse competition, so when i multiply a horse's score
       %by that and add it, it weighs that horse in by a factor of 100%
       RaceBaselineTotal = raceTotal*Baseline; %gets each a representation for each horse at the baseline level
       for y = start:stop %goes through each horse 
           Closestfourtotal = 0; %pre-allocating
           ClosestTwoTotal = 0; %pre-allocating
           FirstHorseTwo = 0;
           LastHorseTwo = 0;
           FirstHorseFour = 0;
           LastHorseFour = 0;
           if y >= start+1 && y <= stop-1 % if horses is between second and second last position wise
               for j = y-1:y+1 % goes through each of the closest two horses
                   ClosestTwoTotal = ClosestTwoTotal + Central(j,31)*SecondTier; %multiplies the closest two horses and adds
               end
           end
           if y == start+1
               for t = y-1:y+3 %goes to next 4 four horses, including self
                   Closestfourtotal = Closestfourtotal + Central(t,31) * FirstTier;
               end
           end
           if y == stop-1
               for t = y+1:-1:y-3 %goes to backwards to next 4 four horses, including self
                   Closestfourtotal = Closestfourtotal + Central(t,31) * FirstTier;
               end
           end
           if y == start %if the first horse
               for d = y:y+2 %goes to next two horses
                   FirstHorseTwo = FirstHorseTwo + Central(d,31)*SecondTier;
               end
               for t = y:y+4 %goes to next 4 four horses, including self
                   FirstHorseFour = FirstHorseFour + Central(t,31) * FirstTier;
               end
           end
           if y == stop
               for q = y:-1:y-2 % goes backwards from last horse, including self
                   LastHorseTwo = LastHorseTwo + Central(q,31) * SecondTier;
               end
               for w = y:-1:y-4
                   LastHorseFour = LastHorseFour + Central(w,31)* FirstTier;
               end
           end
                   if y >= start+2 && y<= stop-2 %if the horse is between third from start and third from last
               for k = y-2:y+2 %goes through the two horses to the left and two horses to the right
                   Closestfourtotal = Closestfourtotal + Central(k,31)*FirstTier; %multiplies each of the closes four and adds to total
               end
                   end
           Central(y,32) = RaceBaselineTotal + ClosestTwoTotal + Closestfourtotal + FirstHorseTwo + FirstHorseFour + LastHorseTwo + LastHorseFour;
       end
       
   end
   
   %this loop will sort the top picks for out the gate/rel. comp. first
   %loop/ give the position prediciton based upon the scores
   for j = 1:NumRaces 
      
      start = NumHorsesPerRaceTable(j,5);
      stop = NumHorsesPerRaceTable(j,3);
      Central(start:stop,:) = sortrows(Central(start:stop,:), 32,'descend'); %sorts rows in descending order based upon the 32nd column which is out the gate, after relative comp
      
  end
  

  
  %RELATIVE COMPETITION PART 2
  
  %This loop will go through and get the competition at the mid part of the
  %race, this is based of the prediciton of the place that each horse is in
  %off the first relative comp loop above, which is also the prediciton for
  %the early/outthegate position., the positin of the sort rows column
  %above is critical becuase it puts the horses in the predicited position
  %based upon their score, then the following loop should factor in the
  %pps and workouts parts.

   for r = 1:NumRaces
       
       start = NumHorsesPerRaceTable(r,5);
       stop =  NumHorsesPerRaceTable(r,3);
       raceTotal = sum(Central(start:stop,7)); %gets sum of scores for each race
       RaceSurfaceTable(r,1) = Tablecell(start,7); %this doesnt affect this loop
       RaceBaselineTotal = raceTotal*Baseline; %gets each a representation for each horse at the baseline level
       for y = start:stop %goes through each horse 
           Closestfourtotal = 0; %pre-allocating
           ClosestTwoTotal = 0; %pre-allocating
           FirstHorseTwo = 0;
           LastHorseTwo = 0;
           FirstHorseFour = 0;
           LastHorseFour = 0;
           if y >= start+1 && y <= stop-1 % if horses is between second and second last position wise
               for j = y-1:y+1 % goes through each of the closest two horses
                   ClosestTwoTotal = ClosestTwoTotal + Central(j,7)*SecondTier; %multiplies the closest two horses and adds
               end
           end
           if y == start+1
               for t = y-1:y+3 %goes to next 4 four horses, including self
                   Closestfourtotal = Closestfourtotal + Central(t,7) * FirstTier;
               end
           end
           if y == stop-1
               for t = y+1:-1:y-3 %goes to backwards to next 4 four horses, including self
                   Closestfourtotal = Closestfourtotal + Central(t,7) * FirstTier;
               end
           end
           if y == start %if the first horse
               for d = y:y+2 %goes to next two horses
                   FirstHorseTwo = FirstHorseTwo + Central(d,7)*SecondTier;
               end
               for t = y:y+4 %goes to next 4 four horses, including self
                   FirstHorseFour = FirstHorseFour + Central(t,7) * FirstTier;
               end
           end
           if y == stop
               for q = y:-1:y-2 % goes backwards from last horse, including self
                   LastHorseTwo = LastHorseTwo + Central(q,7) * SecondTier;
               end
               for w = y:-1:y-4
                   LastHorseFour = LastHorseFour + Central(w,7)* FirstTier;
               end
           end
                   if y >= start+2 && y<= stop-2 %if the horse is between third from start and third from last
               for k = y-2:y+2 %goes through the two horses to the left and two horses to the right
                   Closestfourtotal = Closestfourtotal + Central(k,7)*FirstTier; %multiplies each of the closes four and adds to total
               end
                   end
           Central(y,8) = RaceBaselineTotal + ClosestTwoTotal + Closestfourtotal + FirstHorseTwo + FirstHorseFour + LastHorseTwo + LastHorseFour;
       end
       
   end
   
   for j = 1:NumRaces 
      
      start = NumHorsesPerRaceTable(j,5);
      stop = NumHorsesPerRaceTable(j,3);
      Central(start:stop,:) = sortrows(Central(start:stop,:), 33,'descend'); %sorts rows in descending order based upon the 33rd column which is past performance after relative comp
      
   end
   
   
    for e = 1:Rows %goes through each horse
       Central(e,9) = Central(e,7) / Central(e,8); %divided horses score but total score to get percentage
   end  
   
   
   %Gets the 1 coeffcient for each seperate race, make the values save in the
   %end so they can be analyzed

%the smaller the range between races of similar value the more consistent
%and generally accurate, i think.

AvgCoefTable = zeros(NumRaces,34); %pre-allocating, this table shows  the avg coef for diff races
AvgCoefTableClass = cell(NumRaces,4);
for y = 1:NumRaces
    
    start = NumHorsesPerRaceTable(y,5);
    stop =  NumHorsesPerRaceTable(y,3);
    
    format compact
    %for all values at 1!
    %using median becuase the small sample size is extremely prone to
    %outliers such as horses with no data, I also think that now the horses
    %will be represented at their most meidicore point, and so instead of
    %really weighing insignificant horses scores, this new set of
    %coefficients will instead allow better horses in areas to stand out.
    MorningOddsexp1 = (median(CurrentCoef1(start:stop,1)));
    HorseLife1stwinsurfaceexp1 = (median(CurrentCoef1(start:stop,4)));
    HorseCurYearTop3Recordexp1 = (median(CurrentCoef1(start:stop,5)));
    HorseAtDistTop3Percentageexp1 = (median(CurrentCoef1(start:stop,6)));
    HorseAtTrackTop3Percentageexp1 = (median(CurrentCoef1(start:stop,7)));
    Brisspeedonsurfaceexp1 = (median(CurrentCoef1(start:stop,2)));
    HorseLifeTop3winsurfaceexp1 = (median(CurrentCoef1(start:stop,3)));
    HorseAtDist1stpercentageexp1 = (median(CurrentCoef1(start:stop,8)));
    Populationfinishpredicitonexp1 = (median(CurrentCoef1(start:stop,9)));
    
    AvgCoefTable(y,1) = Table(start,12); %puts the race value in the first column
    AvgCoefTableClass(y,1) = (Tablecell(start,9)); %puts the class in the class table, can pull outside window for easier viewing
    AvgCoefTableClass(y,2) = Tablecell(start,12);
    %THESE show the avg coefficient for each variable to equal one, in each
    %race, wiht different money values, if INF then all the horses values
    %equaled 0, which may men that is is just a maiden race
    AvgCoefTable(y,2) = MorningOddsexp1;
    AvgCoefTable(y,3) = HorseLife1stwinsurfaceexp1;
    AvgCoefTable(y,4) = HorseCurYearTop3Recordexp1;
    AvgCoefTable(y,5) = HorseAtDistTop3Percentageexp1;
    AvgCoefTable(y,6) = HorseAtTrackTop3Percentageexp1;
    AvgCoefTable(y,7) = Brisspeedonsurfaceexp1;
    AvgCoefTable(y,8) = HorseLifeTop3winsurfaceexp1;
    AvgCoefTable(y,9) = HorseAtDist1stpercentageexp1;
    AvgCoefTable(y,10) = Populationfinishpredicitonexp1;
 
    
    HorseAgeinMonthsexp1 = (median(PrepCoef1(start:stop,1)));
    HorseWeigthCarriedexp1 = (median(PrepCoef1(start:stop,2)));
    RaceRestDaysexp1 = (median(PrepCoef1(start:stop,3)));
    Trainertop3winexp1 = (median(PrepCoef1(start:stop,4)));
    Jockeytop3winexp1 = (median(PrepCoef1(start:stop,5)));
    PostPositionexp1 = (median(PrepCoef1(start:stop,6)));
    HorseBlinkerstatusexp1 = (median(PrepCoef1(start:stop,7)));
    WorkRestDaysexp = (median(PrepCoef1(start:stop,8)));
    
    AvgCoefTable(y,11) = HorseAgeinMonthsexp1;
    AvgCoefTable(y,12) = HorseWeigthCarriedexp1;
    AvgCoefTable(y,13) = RaceRestDaysexp1;
    AvgCoefTable(y,14) = Trainertop3winexp1;
    AvgCoefTable(y,15) = Jockeytop3winexp1;
    AvgCoefTable(y,16) = PostPositionexp1;
    AvgCoefTable(y,17) = HorseBlinkerstatusexp1;
    AvgCoefTable(y,18) = WorkRestDaysexp;
   
    workdistexp1 = (median(Workcoef1(start:stop,1)));
    numworksperdayexp1 = (median(Workcoef1(start:stop,2)));
    worksconsistencyaverageexp1 = (median(Workcoef1(start:stop,3)));
    worksavgexp1 = (median(Workcoef1(start:stop,4)));
    
    AvgCoefTable(y,19) = workdistexp1;
    AvgCoefTable(y,20) = numworksperdayexp1;
    AvgCoefTable(y,21) = worksconsistencyaverageexp1;
    AvgCoefTable(y,22) = worksavgexp1;
    
    
    AverageOutperformanceexp1 = (median(PPcoef1(start:stop,1)));
    restdaysexp1 = (median(PPcoef1(start:stop,2)));
    raceoddsexp1 = (median(PPcoef1(start:stop,3)));
    racevalueexp1  = (median(PPcoef1(start:stop,4)));
    NormalizedBrisSpeedexp1 = (median(PPcoef1(start:stop,5)));
    raceweightcarriedexp1 = (median(PPcoef1(start:stop,6)));
    EarlyMarginSpeedexp1 = (median(PPcoef1(start:stop,7)));
    MidMarginSpeedexp1 = (median(PPcoef1(start:stop,8)));
    LateMarginSpeedexp1 = (median(PPcoef1(start:stop,9)));
    MarginTrendexp1 = (median(PPcoef1(start:stop,10)));
    NormalizedLateMarginSpeedexp1 = (median(PPcoef1(start:stop,11)));
    
    AvgCoefTable(y,23) = AverageOutperformanceexp1;
    AvgCoefTable(y,24) = restdaysexp1;
    AvgCoefTable(y,25) = raceoddsexp1;
    AvgCoefTable(y,26) = racevalueexp1;
    AvgCoefTable(y,27) = NormalizedBrisSpeedexp1;
    AvgCoefTable(y,28) = raceweightcarriedexp1;
    AvgCoefTable(y,29) = EarlyMarginSpeedexp1;
    AvgCoefTable(y,30) = MidMarginSpeedexp1;
    AvgCoefTable(y,31) = LateMarginSpeedexp1;
    AvgCoefTable(y,32) = MarginTrendexp1;
    AvgCoefTable(y,33) = NormalizedLateMarginSpeedexp1;
    AvgCoefTableClass(:,4) = RaceSurfaceTable;
    AvgCoefTable(y,34) = RD;
   
    
end

AvgCoefTableClass(:,3) = num2cell(NumHorsesPerRaceTable(:,2)); %puts in the race population, another potential patttern







%running the results automatically
stringRun = 'ResultsAccuracy';
ShortenFILEID1 = char(FileID);
ShortenFILEID2 = ShortenFILEID1(1:7);
RunString = [stringRun ShortenFILEID2 '.m']
run(RunString)
