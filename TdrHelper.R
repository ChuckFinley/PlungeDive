library(ggplot2)
library(dplyr)

# For test purposes, used deploy ids 388, 398, 619

# plot.fastlog returns a ggplot object depicting a portion of a CEFAS fastlog
plot.fastlog <- function(deployid, eventid, start = NULL, span.sec = 5) {
  # Read metadata. This returns two rows: deployment and recovery
  metadata <- read.csv('CEFAS/metadata_all_GPS.csv') %>%
    filter(Deploy_ID == deployid) %>%
    arrange(Tagging_Event)
  
  # Assert exactly two rows found
  if(nrow(metadata) == 0) stop(sprintf('No metadata for Deploy ID %i', deployid))
  if(nrow(metadata) == 1) stop(sprintf('Recovery metadata not found for Deploy ID %i', deployid))
  if(nrow(metadata) > 2) stop(sprintf('Multiple metadata records for Deploy ID %i', deployid))
  
  # Read TDR data and select records from input event
  event <- read.csv(sprintf('CEFAS/%s.CSV', metadata$TDR_File[2]),
                    stringsAsFactors = FALSE) %>%
    filter(EventId == eventid) %>%
    mutate(UTC = as.POSIXct(UTC,
                            tz = 'UTC'))
  
  # Assert event exists
  if(nrow(event) == 0) stop(sprintf('No event %i for Deploy ID %i', eventid, deployid))
  
  # Assert start falls within event
  if(!is.null(start) && !between(start, min(event$UTC), max(event$UTC))) 
    stop(sprintf('Start time %s falls outside of event %i for Deploy ID %i', start, eventid, deployid))
  
  ## Plot event
  # Determine x-axis limits
  event.xlim <- c(0, span.sec) + if(is.null(start)) event$UTC[1] else start
  # Expanded x-axis limits used to make lines "spill" when event is truncated
  expanded.xlim <- event.xlim + c(-1, 1)
  
  # Filter event to within event.xlim
  plot.data <- filter(event, between(UTC, expanded.xlim[1], expanded.xlim[2]))
  
  # Date formatting function for x-axis
  event.date.labels <- function(dates) format(dates, format = '%b %e %H:%M:%OS1')
  
  # Create event plot
  # Date/time on x-axis, pressure on y-axis
  event.plot <- ggplot(plot.data,
                      aes(x = UTC,
                          y = Pressure,
                          group = 1)) +
    # Add lines and points
    geom_line() +
    geom_point() +
    # Set x-axis to display dates every second
    # Fix xlim so events shorter than 1s still display
    scale_x_datetime(date_breaks = '1 sec',
                     labels = event.date.labels,
                     limits = event.xlim) +
    # Flip the y axis so it displays depth instead of pressure
    scale_y_reverse() +
    # Label axes and give plot a title
    labs(x = 'Time',
         y = 'Depth (m)',
         title = sprintf('FieldID = %s\nDeployID = %i, EventID = %i', metadata$FieldID[1], deployid, eventid))
}

