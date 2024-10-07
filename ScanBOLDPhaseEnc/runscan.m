function runscan(project, StartYr,EndYr, StartDate, EndDate)
%function runscan(project, StartYr,EndYr, StartDate, EndDate)
%
% Extract EPI core parameters from project/study/dicom files
% @chm - 07/08/2021
%

%%
%clear all;

%% Porject name to be scanned
%project = 'WPC-7366';

% Range
%StartYr = 2018; EndYr = 2019;
%StartDate = '0101'; EndDate = '0631';

%% Checking
cdt = datetime;

if isempty(StartYr)
    StartYr = year(cdt);
    EndYr = StartYr;
end
if isempty(EndYr)
    EndYr = year(cdt);
    StartYr = EndYr;
end
if StartYr > EndYr
    StartYr = EndYr;
end

if isempty(StartDate)
    mn = month(cdt);
    dy = day(cdt);
    StartDate = sprintf('%02d%02d', mn, dy);
    EndDate = StartDate;
end

if isempty(EndDate)
    mn = month(cdt);
    dy = day(cdt);
    EndDate = sprintf('%02d%02d', mn, dy);
    StartDate = EndDate;
end

%%
project = upper(project);
projectfilenamenew = [project '_BOLDDWI_' num2str(StartYr) StartDate '-' num2str(EndYr) EndDate '.xlsx'];

%@chm - 2022/11/16
EPItype = 'BOLD'; %'BOLD'; %'DWI' % ;; Not used!

%% Run
icnt = 0;
mcellLineNew=[];
for yr=StartYr:EndYr
    % archive folder and date range
    if yr < 2023
        pfolderlist = {['/disk/mace2/archive/scan_data_archive/' num2str(yr)]};
    else
        pfolderlist = {['/disk/mace2/scan_data/' num2str(yr)]};
        TF = isfolder(pfolderlist);
        if TF~=1
            pfolderlist = {'/disk/mace2/scan_data'};
        end
    end
    rangestudydate = [num2str(yr) StartDate; num2str(yr) EndDate];
    
    tt = ['Scanning ' '"' pfolderlist{1,1} '" from "' rangestudydate(1,:) '" to "' rangestudydate(2,:) '"'];
    cprintf(-[1,0,1], '%s\n', tt);
    
    % scan DICOM  files
    [tableLine,projectfilename,Label] = extractepiped_func2(project, pfolderlist,rangestudydate, EPItype);
    nrow = size(tableLine,1);
    if nrow < 2
       continue; 
    end
    
    % b0 shim decision
    [tableLineNew] = decisionB0ShimSuccess(tableLine,projectfilename);
    
    % merge
    cellLineNew = table2cell(tableLineNew);
    if icnt == 0
        mcellLineNew = [mcellLineNew; cellLineNew(1:end,:)];
    else
        mcellLineNew = [mcellLineNew; cellLineNew(2:end,:)];
    end
    
    % counting
    icnt = icnt + 1;
end

%% excel writing
if ~isempty(mcellLineNew)
    Label = [Label 'B0ShimSuccess'];
    mtableLineNew = cell2table(mcellLineNew,'VariableNames',Label); %table labeling in MATLAB
    rangestr = ['A' '1'];
    writetable(mtableLineNew,projectfilenamenew,'Sheet',tableLineNew.Project{2,1},'Range',rangestr,'WriteRowNames',true,'WriteVariableNames', 0);

    cprintf(-[1,0,1], 'Press "Cntrl+C"\n');
end

%%
return;
