function [ped,s] = readped(fname)
% function [ped,s] = readped(fname)
% @chm - 03/27/2021
%

% read DICOM header including CSA information
[s, err] = dicm_hdr(fname);
    
PELin = upper(s.InPlanePhaseEncodingDirection);
PEPol = s.CSAImageHeaderInfo.PhaseEncodingDirectionPositive;
        
if strcmp(PELin,'ROW')
    if PEPol==1
        %disp([fname ': PED ' 'R > L']);
        %fprintf(1, '%s: ', fname); fprintf(2, 'PED R > L\n');
        ped = 'R > L';
    else
        %disp([fname ': PED ' 'L > R']);
        %fprintf(1, '%s: ', fname); fprintf(2, 'PED L > R\n');
        ped = 'L > R';
    end
elseif strcmp(PELin,'COL')
    if PEPol==1
        %disp([fname ': PED ' 'A > P']);
        %fprintf(1, '%s: ', fname); fprintf(2, 'PED A > P\n');
        ped = 'A > P';
    else
        %disp([fname ': PED ' 'P > A']);
        %fprintf(1, '%s: ', fname); fprintf(2, 'PED P > A\n');
        ped = 'P > A';
    end
else
    %disp('No direction indicated!');
    ped = 'NONE';
end

return;