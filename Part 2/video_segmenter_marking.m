clear

%% Choose Time Segments or Random Timepoints 

name = input('Name (First_Last): ', 's'); 
colony = input('Colony ID ','s'); 
date = input('Video Date (day_month_year) ','s'); 

ants = input('How many ants? ');

seconds_string = 'seconds'; 
csv = '.csv';
tasks = 'tasks';
underscore = '_'; 
segment = 'segment'; 
space = 'space'; 
frames = 'frames'; 
file_name_seconds = [name underscore colony underscore date underscore seconds_string];
file_name_seconds_data = [name underscore colony underscore date csv];
file_name_frames = [name underscore  colony underscore date underscore frames]; 

sampling_choice = "F"; 

v = VideoReader('17_1.mp4');

total_minutes = v.Duration/60; 

for i = 1:ants
   antName = input('Ant ID: ', 's'); 
   antNames{i} = antName;
end

if sampling_choice == 'F'

    minutes = input('How many minutes per ant? ');
    segments = input('How many segments will you watch? '); 

    minutes_per_seg = minutes/segments; 
    success = 0; 

    counter = 0; 
    check = 0; 

    while success == 0 

        segment_starts = round(rand(segments, 1)*(total_minutes-minutes_per_seg), 4); 
        segment_ends = round(segment_starts + minutes_per_seg, 4); 

        if segments > 1 

            for i = 1:segments 
                for j = 1:segments
                    difference(i, j) = segment_ends(i) - segment_starts(j); 
                end
            end

            S = sign(difference);
            neg_counter = sum(S(:) == -1); 

            if neg_counter < (segments*(segments-1)/2)
                success = 0;
            else 
                success = 1;
            end

            counter = counter + 1; 

            if counter > 1000
                disp("Error: No Possible Solution. Decrease number of minutes sampled and/or increase the total number of minutes for video")
                success = 1;
                check = 1; 
            end

        else
            success = 1;
        end
    end

    if check == 0 

        segment_starts = sort(segment_starts); 
        segment_ends = sort(segment_ends); 

        seconds_starts = floor((segment_starts - floor(segment_starts)).*60); 
        seconds_ends = floor((segment_ends - floor(segment_ends)).*60); 

        minutes_starts = floor((segment_starts/60 - floor(segment_starts/60)).*60); 
        minutes_ends = floor((segment_ends/60 - floor(segment_ends/60)).*60); 

        hours_starts = floor(segment_starts/60); 
        hours_ends = floor(segment_ends/60); 

        X = [hours_starts, repelem(":", segments)', minutes_starts, repelem(":", segments)', seconds_starts, repelem("  ->  ", segments)', ...
            hours_ends, repelem(":", segments)', minutes_ends, repelem(":", segments)', seconds_ends]; 
        
        total_seconds_starts = seconds_starts+60.*minutes_starts+60.*60.*hours_starts; 
        total_seconds_ends = seconds_ends+60.*minutes_ends+60.*60.*hours_ends; 
        
        time_vector = []; 
        
        for i = 1:segments
            X1{i, :} = strjoin(X(i, :)); 
            time_vector = [time_vector total_seconds_starts(i):total_seconds_ends(i)]; 
        end

        disp("Sample Ranges") 
        disp(X1)
        
        segment_vector = []; 
        for i = 1:segments
            segment_vector = [segment_vector floor(segment_starts(i).*60):floor(segment_ends(i).*60)]; 
        end
        
    end
    
elseif sampling_choice == 'T'
    
    minutes = input('How many minutes per ant? ');
    seconds = minutes.*60; 
    total_seconds = total_minutes.*60; 
    segment_vector = floor(rand(1, seconds)*total_seconds); 
    
    X1 = {"Random"}; 
    
else
    
    disp("Error: Type in T or F for Sampling Choice")

end

%% Export segmented videos

video_segmented = VideoWriter(file_name_seconds,'MPEG-4'); 

for i = 1:length(segment_vector)
   
    obj = read(v, round(segment_vector(i)*v.FrameRate));
    open(video_segmented)
    writeVideo(video_segmented,obj)
    
end

close(video_segmented)

if sampling_choice == 'F'

    video_segmented_frames = VideoWriter(file_name_frames,'MPEG-4'); 
    segment_starts_frames = segment_starts.*60.*(v.FrameRate./1); 
    segment_ends_frames = segment_ends.*60.*(v.FrameRate./1); 
    segment_vector_frames = []; 
    
     for i = 1:segments
            segment_vector_frames = [segment_vector_frames floor(segment_starts_frames(i)):floor(segment_ends_frames(i))]; 
     end

    for i = 1:length(segment_vector_frames)

        obj = read(v, segment_vector_frames(i));
        open(video_segmented_frames)
        writeVideo(video_segmented_frames,obj)

    end

    close(video_segmented_frames)
    
    for i = 1:length(segment_vector)
        for j = 1:ants
            names_mat{i, j} = name; 
            colony_mat{i, j} = colony; 
            date_mat{i, j} = date; 
            s = seconds(segment_vector(i)); 
            s.Format = 'hh:mm:ss'; 
            timeInVid = seconds(i); 
            timeInVid.Format = 'hh:mm:ss'; 
            time_mat{i, j} = s; 
            task_mat{i, j} = {}; 
            timeInVidMat{i,j} = timeInVid;
        end
    end
    
    diff_vector = diff(segment_vector); 
    segment_index = find(diff_vector>1)+1; 
    segment_index = [1 segment_index];
    segments_breaks = segment_vector(segment_index); 
    
    counter = 0; 
    counter_segments = 0; 
    for j = 1:ants
        for i = 1:length(segment_vector)
            if isempty(segments_breaks(segments_breaks==segment_vector(i))) == 0
                counter = counter + 1;
                if mod(counter, segments) == 1
                    counter_segments = 1; 
                else
                    counter_segments =  counter_segments + 1; 
                end
            end
            ant_id_mat{i, j} = counter;
            ant_paint_mat{i, j} = antNames{j}; 
            segment_mat{i, j} = counter_segments; 
        end
    end
    
end

Analyst = names_mat(:); 
Colony = colony_mat(:); 
AntID = ant_paint_mat(:); 
Time = time_mat(:); 
%Task = task_mat(:); 
Date = date_mat(:); 
SegmentNumber = segment_mat(:); 
TimeInVideo = timeInVidMat(:); 

%T = table(Analyst, Colony, Date, AntID, SegmentNumber, Time, Task);
T = table(Analyst, Colony, Date, AntID, SegmentNumber, Time, TimeInVideo);
writetable(T,file_name_seconds_data) 
