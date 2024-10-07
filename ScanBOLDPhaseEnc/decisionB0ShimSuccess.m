function [tableLineCopy] = decisionB0ShimSuccess(tableLine,projectfilename)
% function decisionB0ShimSuccess(tableLine,projectfilename)
% @chm - 08/02/2021
%

% buffering
DateTime = tableLine.DateTime(2:end,:);
SubID = tableLine.SubID(2:end,:);
PED = tableLine.PED(2:end,:);
SeqMode = tableLine.SeqMode(2:end,:);
ShimValues = tableLine.B0Shim(2:end,:);
Sequence = tableLine.Sequence(2:end,:);    
PED = tableLine.PED(2:end,:);    
ShimMode = tableLine.ShimMode(2:end,:);    

% copy tableLine
tableLineCopy = tableLine;

% new column for B0 shim success
B0ShimSuccess = cell(size(tableLine,1),1);
B0ShimSuccess{1,1} = 'B0ShimSuccess';

% find the study group
uniqueDateTime = unique(DateTime);
uniqueSubID = unique(SubID);
for i=1:length(uniqueDateTime)
    strdatetime = uniqueDateTime{i};
    idatetime = [];
    ntable = size(DateTime,1);
    for j=1:ntable
        idx = strcmp(strdatetime,DateTime{j});
        if idx==1
            idatetime = [idatetime j];
        end
    end
    %disp(idatetime)

    % per study
    studySeqMode = SeqMode(idatetime);
    studySequence = Sequence(idatetime);
    studyShimValues = ShimValues(idatetime);
    studyPED = PED(idatetime);
    studyShimMode = ShimMode(idatetime);

    % checking BOLD B0 shim 
    idc = strfind(studySeqMode,'BOLD');
    idxbold = find(~cellfun('isempty',idc));
    idc = strfind(studySeqMode,'SE');
    idxSE = find(~cellfun('isempty',idc));
    idc = strfind(studySeqMode,'GRFM');
    idxGRFM = find(~cellfun('isempty',idc));
    %idc = strfind(studyShimMode,'Advanced');
    %idxShimModeAdvanced = find(~cellfun('isempty',idc));
    
    for k=1:length(idxbold)
        boldshim = studyShimValues(idxbold(k));
        boldshimmode = studyShimMode(idxbold(k));
        % SE fw & bw field mapping
        cntseshim = 0;
        for m=1:length(idxSE)
            %seseqmode = studySeqMode(idxSE(m));
            seshim = studyShimValues(idxSE(m));
            seshimmode = studyShimMode(idxSE(m));
            %{
            if strcmp(boldshim,seshim)
                cntseshim = cntseshim + 1;
            end
            %}
            %if strcmp(boldshimmode,"Advanced") || strcmp(seshimmode,"Advanced")
                if strcmp(boldshim,seshim)
                    cntseshim = cntseshim + 1;
                end
            %end
        end
        %disp(cntseshim)
        % GRFM
        cntgrfmshim = 0;
        for m=1:length(idxGRFM)
            %seseqmode = studySeqMode(idxGRFM(m));
            grefmshim = studyShimValues(idxGRFM(m));
            grefmshimmode = studyShimMode(idxGRFM(m));
            %{
            if strcmp(boldshim,grefmshim)
                cntgrfmshim = cntgrfmshim + 1;
            end
            %}
            %if strcmp(boldshimmode,"Advanced") || strcmp(grefmshimmode,"Advanced")
                if strcmp(boldshim,grefmshim)
                    cntgrfmshim = cntgrfmshim + 1;
                end
            %end
        end

        % report
        if cntseshim==2 || cntgrfmshim==1
            %disp([studySequence(idxbold(k)) '- SUCCESS']);
            B0ShimSuccess{idatetime(idxbold(k))+1,1} = 'Success';
        else
            B0ShimSuccess{idatetime(idxbold(k))+1,1} = 'Failure';
        end
        
    end
    
    % checking DWI B0 shim 
    idc = strfind(studySeqMode,'DWI');
    idxdwi = find(~cellfun('isempty',idc));
    for k=1:length(idxdwi)
        seq1 = studySequence(idxdwi(k));
        ped1 = studyPED(idxdwi(k));
        shim1 = studyShimValues(idxdwi(k));
        shimmode1 = studyShimMode(idxdwi(k));
        for p=1:length(idxdwi)
            seq2 = studySequence(idxdwi(p));
            ped2 = studyPED(idxdwi(p));
            shim2 = studyShimValues(idxdwi(p));
            shimmode2 = studyShimMode(idxdwi(p));
            
            if ~strcmp(seq1,seq2) && ~strcmp(ped1,ped2) % different sequence & PED
               if strcmp(shim1,shim2) % same shim values
                   B0ShimSuccess{idatetime(idxdwi(k))+1,1} = 'Success';
                   break;
               else
                   B0ShimSuccess{idatetime(idxdwi(k))+1,1} = 'Failure';
               end
            else
               B0ShimSuccess{idatetime(idxdwi(k))+1,1} = 'Failure';
            end
            %{
            if strcmp(shimmode1,"Advanced") || strcmp(shimmode2,"Advanced")
                if ~strcmp(seq1,seq2) && ~strcmp(ped1,ped2) % different sequence & PED
                   if strcmp(shim1,shim2) % same shim values
                       B0ShimSuccess{idatetime(idxdwi(k))+1,1} = 'Success';
                       break;
                   else
                       B0ShimSuccess{idatetime(idxdwi(k))+1,1} = 'Failure';
                   end
                else
                   B0ShimSuccess{idatetime(idxdwi(k))+1,1} = 'Failure';
                end
            else
                B0ShimSuccess{idatetime(idxdwi(k))+1,1} = 'Failure';
            end
            %}
        end
    end

    % checking SE B0 shim 
    idc = strfind(studySeqMode,'SE');
    idxse = find(~cellfun('isempty',idc));
    for k=1:length(idxse)
        seq1 = studySequence(idxse(k));
        ped1 = studyPED(idxse(k));
        shim1 = studyShimValues(idxse(k));
        for p=1:length(idxse)
            seq2 = studySequence(idxse(p));
            ped2 = studyPED(idxse(p));
            shim2 = studyShimValues(idxse(p));
            if ~strcmp(seq1,seq2) && ~strcmp(ped1,ped2) % different sequence & PED
               if strcmp(shim1,shim2) % same shim values
                   B0ShimSuccess{idatetime(idxse(k))+1,1} = 'Success';
                   break;
               else
                   B0ShimSuccess{idatetime(idxse(k))+1,1} = 'Failure';
               end
            else
               B0ShimSuccess{idatetime(idxse(k))+1,1} = 'Failure';
            end
        end
    end
    
    
end

tableLineCopy = addvars(tableLineCopy,B0ShimSuccess);

% excel writing
rangestr = ['A' '1'];
writetable(tableLineCopy,projectfilename,'Sheet',tableLine.Project{2,1},'Range',rangestr,'WriteRowNames',true,'WriteVariableNames', 0);
