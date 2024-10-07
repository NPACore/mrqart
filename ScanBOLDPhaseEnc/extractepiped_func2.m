function [tableLine,projectfilename,Label] = extractepiped_func2(project,pfolderlist,rstuydates,EPItype)
%function [tableLine,projectfilename,Label] = extractepiped_func2(project,pfolderlist,rstuydates,EPItype)
% Scan studies to extract diffusion table per protocol
% @chm - 03/25/2021
%
% Input;
%   currproject - project names (cell)
%   pfoler - data archving folder (string)
%   rstuydates - range of study dates (array)
%

tableLine = [];

%% set disregarded folder name in scan_data
rmfolder = '7T; CLIN-SCAN; homeless; temp; .DS_Store; ._.DS_Store;';

%% set DICOM file naming 
dicomname = {...
    'MR.'...
    '.DCM'...
    '.IMA'
    };
Nonedicomname = {...
    '.results'...
    '.batch.'...
    };
ASLseqname = {...
    'pcasl',...
    'pasl',...
    'asl',...
    };

%% Processing
Label = {...
'StudyDate',...
'Scanner',...
'ScanData',...
'Project',...
'DateTime',...
'SubID',...
'Sequence',...
'PED',...
'TR',...
'TE',...
'Matrix',...
'PixelResol',...
'BWP',...
'BWPPE',...
'FA',...
'TA',...
'FoV',...
'iPAT',...
'MB',...
'FoVShift',...
'SlcLeakBlk',...
'SeqMode',...
'SequenceName',...
'Coil',...
'Provider',...
'ShimMode',...
'B0Shim',...
'Operator',...
};
nLabel = length(Label);


%%
% dates for scanning
if ~isempty(rstuydates)
    startdate = str2num(rstuydates(1,:));
    enddate = str2num(rstuydates(2,:));
    if startdate > enddate
        tt = startdate;
        startdate = enddate;
        enddate = tt;
    end
end

% excel file
%projectfilename = [project '_BOLDDWISheet.xlsx'];
if ~isempty(rstuydates)
    projectfilename = [project '_BOLDDWI_' rstuydates(1,:) '-' rstuydates(2,:) '.xlsx'];
else
    projectfilename = [project '_BOLDDWI.xlsx'];
end

while exist(projectfilename, 'file')
    projectfilename = [projectfilename(1:end-5) '_1.xlsx']; 
end

% cell preparation
cellLine = [];
cellLine = [cellLine; Label]; %table labeling in EXCEL
        
ntableline = 1;

%% >>>> ARCHIVE FOLDER <<<<<<

grefieldmapshimvalues = [];

scandatebuff = [];

npfolders = length(pfolderlist);
for ipfolder=1:npfolders

    % folder path
    pfolder = pfolderlist{1,ipfolder};
    fullname = [pfolder '/' project];
    if ~isfolder(fullname), continue; end
        
    % checking if current folder is in disregarded folders
    if strfind(rmfolder,project), continue; end
    
    % scan all subfolders & (dicom) files
    strcommand = ['find ' fullname ' -type f -printf "%T@ %p\n" | sort -nr | cut -d\  -f2-'];
    [status, result] = system(strcommand);
    if isempty(result), continue; end
    NL = regexp(result, '[\n]');
    
    %disp([fullname ': ' num2str(length(NL))]);
    
    % PER DICOM FILE 
    scannameprev = [];
    
    for j=1:length(NL)-1
        
        % checking dicom file naming
        if j==1
            fname = result(1:NL(j)-1);
            Islash = strfind(fname,'/');
            scanname = fname(1:Islash(end)-1);
        else
            fname = result(NL(j)+1:NL(j+1)-1);
            Islash = strfind(fname,'/');
            scanname = fname(1:Islash(end)-1);
            if strcmp(scannameprev, scanname), continue; end
        end
        
        % checking if the file is DICOM format
        nodicom=0;
        for ii=1:length(dicomname)
            if ~isempty(strfind(upper(fname),dicomname{1,ii}))
                nodicom = 1;
            end
        end
        if nodicom==0, scannameprev=scanname; continue; end
        % one more checking!!
        nodicom=1;
        for ii=1:length(Nonedicomname)
            if ~isempty(strfind(upper(fname), upper(Nonedicomname{1,ii})))
                nodicom = 0;
            end
        end
        if nodicom==0, scannameprev=scanname; continue; end

        % skipping, not interested
        if strfind(fname,'PhoenixZIPReport'), scannameprev=scanname; continue; end
        if strfind(fname,'MoCoSeries'), scannameprev=scanname; continue; end
        if strfind(fname,'Perfusion_Weighted'), scannameprev=scanname; continue; end
        if strfind(fname,'relCBF_'), scannameprev=scanname; continue; end
        if strfind(fname,'Mean_&_t-Maps_'), scannameprev=scanname; continue; end
        if strfind(fname,'EvaSeries_GLM_'), scannameprev=scanname; continue; end
        if strfind(fname,'Design_'), scannameprev=scanname; continue; end
        if strfind(fname,'intermediate-t-Map_'), scannameprev=scanname; continue; end
        if strfind(fname,'StartFMRI_'), scannameprev=scanname; continue; end
        if strfind(fname,'_TRACEW_'), scannameprev=scanname; continue; end
        if strfind(fname,'_FA_'), scannameprev=scanname; continue; end
        if strfind(fname,'_ADC_'), scannameprev=scanname; continue; end
        if strfind(fname,'_SBRef_'), scannameprev=scanname; continue; end
        if strfind(fname,'_vNav_setter_'), scannameprev=scanname; continue; end   
                
        % reading DICOM header                    
        try 
            info = dicominfo(fname);
        catch
            fprintf(2, 'NO DICOM file %s\n', fname);
            scannameprev=scanname; 
            continue;
        end
        
        % checking if the sequence is EPI sequence
        if ~isfield(info,'ScanningSequence'), scannameprev=scanname; continue; end
       
        %disp([info.SeriesDescription info.SequenceName]);
        
        %{
        % gre field mapping
        if strfind(info.SequenceName, '*fm2d2r')
            [grefieldmapshimvalues,strbuff] = readshimvalues(fname);
        end
        %}
        
        % EPI sequence
        strLine = [];
        
        
        %{
        %@chm - 11/18/2022 ... in working!
        Head_T2w_SPC1_320x300
        'SE';
        '*spc_171ns';
        %}
        
        iscanseq = strfind(info.ScanningSequence,'EP');
        iseqname = strfind(info.SequenceName,'*fm2d2r');
        
        %@chm - 11/18/2022
        iSPACEseqname = strfind(info.SequenceName,'*spc_171ns');
        irecon = strfind(info.SeriesDescription,'recon');
        if ~isempty(irecon)
            iSPACEseqname = [];
        end
                
        %if ~isempty(iscanseq) || ~isempty(iseqname)
        if ~isempty(iscanseq) || ~isempty(iseqname) || ~isempty(iSPACEseqname)
               
            % checking the date for scanning
            if ~isempty(rstuydates)
                try
                    studydate = str2num(info.StudyDate);
                    if (studydate < startdate || studydate > enddate)
                        scannameprev = scanname;
                        continue; 
                    end
                catch
                    scannameprev = scanname;
                    continue;
                end
            end
            
            % ... from info dicom header
            if isfield(info,'StationName')
                strLine = [strLine sprintf('%s\t%s\t',info.StudyDate, info.StationName)];
            else
                strLine = [strLine sprintf('%s\t%s\t',info.StudyDate, 'NA')];
            end
           
            % checking different folder naming
            scanname1 = erase(scanname, pfolder);
            rscanname = replace(scanname1(2:end),'/',char(9));
            rscannamecell = strsplit(rscanname,'\t');
            ncell_ = length(rscannamecell);
            rscanname1 = rscannamecell{1:1};
            if ncell_ > 4
                for jj=ncell_-4+2:ncell_
                    rscanname1 = [rscanname1 char(9) rscannamecell{1,jj}];
                end
            else
                for jj=2:4
                    rscanname1 = [rscanname1 char(9) rscannamecell{1,jj}];
                end
            end
            
            % shim
            [EPIshimvalues,strbuff] = readshimvalues(fname);
            
            % ... from special dicom info
            [EPI_PED, S] = readped(fname);
            strLine = [strLine sprintf('%s\t%s\t%s\t',pfolder,rscanname1, EPI_PED)];
            
            % ... other parameters
            TR = info.RepetitionTime;
            TE = info.EchoTime;
            strLine = [strLine sprintf('%3.2f\t%3.2f\t',TR,TE)];
                                
            AcquisitionMatrix = info.AcquisitionMatrix;
            MatrixSize = [];
            for i=1:length(AcquisitionMatrix)
                if AcquisitionMatrix(i)~=0, 
                    MatrixSize = [MatrixSize; AcquisitionMatrix(i)]; 
                end
            end
            strLine = [strLine sprintf('%d',MatrixSize(1))];
            for i=2:length(MatrixSize)
                strLine = [strLine sprintf('x%d',MatrixSize(i))];
            end
            strLine = [strLine sprintf('\t')]; 
                
            PixelSpacing = info.PixelSpacing;
            SliceThickness= info.SliceThickness;
            strLine = [strLine sprintf('%3.2fx%3.2fx%3.2f\t',PixelSpacing(1),PixelSpacing(2),SliceThickness)];
                
            PixelBandwidth = info.PixelBandwidth;
            strLine = [strLine sprintf('%3.2f\t',PixelBandwidth)];
            try 
                BandwidthPerPixelPhaseEncode = S.BandwidthPerPixelPhaseEncode;
            catch
                BandwidthPerPixelPhaseEncode = 0;
            end
            strLine = [strLine sprintf('%3.2f\t',BandwidthPerPixelPhaseEncode)];
                
            FlipAngle = info.FlipAngle;
            strLine = [strLine sprintf('%3.2f\t',FlipAngle)];
                
            TA = info.Private_0051_100a; %Private_0051_100a: 'TA 04:16'
            TA = strtrim(erase(TA,'TA'));
            strLine = [strLine sprintf('%s\t',TA)];
            
            Fov = info.Private_0051_100c; %Private_0051_100c: 'FoV 210*210'
            Fov = replace(strtrim(erase(Fov,'FoV')),'*','x');
            FoVx = MatrixSize(1)*PixelSpacing(1);
            FoVy = MatrixSize(2)*PixelSpacing(2);
            strLine = [strLine sprintf('%3.2fx%3.2f\t',FoVx,FoVy)];
                
            try 
                ImaPATModeText = S.ImaPATModeText; %ImaPATModeText: 'p2'; %ImaPATModeText: 's6'
                ip = strfind(ImaPATModeText,'p');
                iPAT = str2num(ImaPATModeText(ip+1));
                if isempty(iPAT)
                    iPAT = 1;
                end
            catch
                iPAT = 1;
            end
            strLine = [strLine sprintf('%d\t',iPAT)];
                
            try 
                ImageTypeText = S.ImageTypeText; %ImageTypeText: 'p2 M/MB/TE1/ND/NORM/MOSAIC'; %ImageTypeText: 's6 M/ND/MOSAIC'
            catch
                ImageTypeText = 'None';
            end
            %strLine = [strLine sprintf('%s\t',ImageTypeText)];
                
            try 
                ImageComments = S.ImageComments; %ImageComments: 'Unaliased MB6/PE4 SENSE1'; %ImageComments: 'SMS_ABCD_V1.2; MB=6; FOVshift=3; K_PE=5; K_RO=5; LeakBlockOn;'
                isABCD = strfind(ImageComments,'ABCD');
                % CMRR SMS MB sequences
                if isempty(isABCD) 
                    iMB = strfind(ImageComments,'MB');
                    iFovShift = strfind(ImageComments,'PE');
                    ibSlash = strfind(ImageComments,'/');
                    if ~isempty(iMB)
                        MB = str2num(ImageComments(iMB+2:ibSlash-1));
                    else
                        MB = 1;
                    end
                    if ~isempty(iFovShift)
                        FovShift = str2num(ImageComments(iFovShift+2));
                    else
                        FovShift = 1;
                    end
                        
                    % ... will be modified
                    if ~isempty(strfind(ImageComments,'LB'))
                        LeakBlock = 'ON';
                    else
                        LeakBlock = 'OFF';
                    end
                        
                    C2P = 'CMRR';
                % ABCD SMS MB sequences
                else 
                    iMB = strfind(ImageComments,'MB=');
                    iFovShift = strfind(ImageComments,'FOVshift=');
                    if ~isempty(iMB)
                        strtmp = ImageComments(iMB:end);
                        isColon = strfind(strtmp,';');
                        MB = str2num(strtmp(length('MB=')+1:isColon(1)-1));
                    else
                        MB = 1;
                    end
                    if ~isempty(iFovShift)
                        strtmp = ImageComments(iFovShift:end);
                        isColon = strfind(strtmp,';');
                        FovShift = str2num(strtmp(length('FOVshift=')+1:isColon(1)-1));
                    else
                        FovShift = 1;
                    end
                                                
                    if ~isempty(strfind(ImageComments,'LeakBlockOn'))
                        LeakBlock = 'ON';
                    else
                        LeakBlock = 'OFF';
                    end
                        
                    C2P = 'ABCD';
                end
            catch
                % Siemens SMS MB sequence
                SequenceName = info.SequenceName;
                if ~isempty(strfind(SequenceName,'*'))
                    try 
                        ImaPATModeText = S.ImaPATModeText; %ImaPATModeText: 's6'
                        islc = strfind(ImaPATModeText,'s');
                        MB = str2num(ImaPATModeText(islc+1));
                        if isempty(MB)
                            MB = 1;
                        end
                    catch
                        MB = 1;
                    end
                    FovShift = 0;
                    LeakBlock = '-';
                    C2P = 'SIEMENS';
                else
                    MB = 1;
                    FovShift = 1;
                    LeakBlock = '-';
                    C2P = '-';
                end
            end
            strLine = [strLine sprintf('%d\t%d\t%s\t',MB,FovShift,LeakBlock)];
             
            SequenceName = info.SequenceName;
            if ~isempty(strfind(SequenceName,'*'))
                C2P = 'SIEMENS';
            end
                
            if ~isempty(strfind(SequenceName,'epfid')) %BOLD
                SeqMode = 'BOLD';
                for jj=1:length(ASLseqname)
                    aslseq = ASLseqname{1,jj};
                    if ~isempty(strfind(lower(info.SeriesDescription), lower(aslseq))) %ASL
                        SeqMode = 'ASL';
                    end
                end
            elseif ~isempty(strfind(SequenceName,'ep_b')) %DWI
                SeqMode = 'DWI';
            elseif ~isempty(strfind(SequenceName,'epse')) %SE
                SeqMode = 'SE';
            elseif ~isempty(strfind(SequenceName,'tgse')) %TGSE asl
                SeqMode = 'TGSE';
            elseif ~isempty(strfind(SequenceName,'*fm2d2r')) %GRE field mapping
                SeqMode = 'GRFM';
            elseif ~isempty(strfind(SequenceName,'*spc_171ns')) %T2SPACE
                SeqMode = 'T2SPACE';
            else
                SeqMode = '-';
            end
            
            %@chm - 11/16/2022
            %{
            if bBOLD==1
                if ~strcmp(SeqMode, 'BOLD')
                    %fprintf(2, 'NonBOLD - %s\n', fname);
                    continue;
                end
                idxfind = strfind(fname,'TRUST');
                if ~isempty(idxfind)
                    %fprintf(2, 'TRUST - %s\n', fname);
                    continue;
                end
            end
            %}
                        
            %
            %if strcmp(EPItype,SeqMode)~=1
            %    continue;
            %end
            
            strLine = [strLine sprintf('%s\t',SeqMode)];
            strLine = [strLine sprintf('%s\t',SequenceName)];
            ImaCoilString = S.CSAImageHeaderInfo.ImaCoilString;
            strLine = [strLine sprintf('%s\t',ImaCoilString)];
            strLine = [strLine sprintf('%s\t',C2P)];

            % B0 shim values
            idxfind = strfind(fname,'PASL_256x256.23');
            %if ~isempty(idxfind)
            %    fprintf(2, 'Failed retrieving shim information - %s', fname);
            %end
            
            [shimvalues,shimmode, strbuff] = readshimvalues(fname);
            switch shimmode
                case 1
                    strLine = [strLine sprintf('TuneUp\t')];
                case 2
                    strLine = [strLine sprintf('Standard\t')];
                case 3
                    strLine = [strLine sprintf('Standard Neck\t')];
                case 4
                    strLine = [strLine sprintf('Advanced\t')];
                case 5
                    strLine = [strLine sprintf('Cardiac\t')];
                case 6
                    strLine = [strLine sprintf('Brain\t')];
                case 7
                    strLine = [strLine sprintf('Breast\t')];
                case 8
                    strLine = [strLine sprintf('Prostate\t')];
                case 9
                    strLine = [strLine sprintf('Foot/Ankle\t')];
                case 10
                    strLine = [strLine sprintf('Abdomen\t')];
                otherwise
                    strLine = [strLine sprintf('TuneUp\t')];
            end
            %strLine = [strLine sprintf('%s\t',num2str(shimmode))];
            shimvalues = num2str(shimvalues(1:8));
            strLine = [strLine sprintf('%s\t',shimvalues)];
            
            % Operator
            if isfield(info,'OperatorName')
                strLine = [strLine sprintf('%s',info.OperatorName.FamilyName)];
            else
                strLine = [strLine sprintf('Unknown')];
            end
        end
                
        % checking cell length is same as label
        if ~isempty(strLine)
            
            strLine2 = strLine;
            strLine = regexp(strLine, '\t', 'split');
            
            % checking duplication
            skipline = 0;
            for irow=1:size(cellLine,1)
               strtmp = [];
               for icol=1:size(cellLine,2)
                   strtmp = [strtmp cellLine{irow,icol}];
               end
               tf1 = strfind(strtmp,strLine{5});
               tf2 = strfind(strtmp,strLine{7});
               
               if ~isempty(tf1) && ~isempty(tf2)
                   %disp(['  ' strLine2]);
                   skipline = 1;
                   break;
               end
            end
            if skipline==1, 
                scannameprev = scanname;
                continue;
            end
            
            % update
            disp([strLine2]);
            
            if length(strLine) > nLabel
                nd = length(strLine) - nLabel;
                cellLine = [cellLine; strLine(1,1:6) strLine(1,6+1+nd:end)];
            elseif length(strLine) < nLabel
                nd = length(strLine) - nLabel;
                cellLine = [cellLine; strLine(1,1:6) zeros(1,-nd) strLine(1,7:end)];
            else
                cellLine = [cellLine; strLine(1,1:nLabel)];
            end
        end
        
        % previous folder name
        scannameprev = scanname;
    end
    
    %{
    % table
    tableLine = cell2table(cellLine,'VariableNames',Label); %table labeling in MATLAB

    % excel writing
    nrow = size(tableLine,1);
    if nrow > 1
        rangestr = ['A' num2str(ntableline)];
        if ntableline==1
            writetable(tableLine,projectfilename,'Sheet',tableLine.Project{2,1},'Range',rangestr,'WriteRowNames',true,'WriteVariableNames', 0);
            ntableline = ntableline + size(tableLine,1);
        else
            writetable(tableLine(2:end,:),projectfilename,'Sheet',tableLine.Project{2,1},'Range',rangestr,'WriteRowNames',true,'WriteVariableNames', 0);
            ntableline = ntableline + size(tableLine,1)-1;
        end
    end
    %}
        
    
end

% table
tableLine = cell2table(cellLine,'VariableNames',Label); %table labeling in MATLAB

% checking
nrow = size(tableLine,1);
if nrow < 2
    fprintf(2,'No EPI sequences are found on ''%s''\n', project);
    return; 
else
    fprintf(1,'Saving ''%s''\n', projectfilename);
end

% excel writing
rangestr = ['A' '1'];
writetable(tableLine,projectfilename,'Sheet',tableLine.Project{2,1},'Range',rangestr,'WriteRowNames',true,'WriteVariableNames', 0);


return;

