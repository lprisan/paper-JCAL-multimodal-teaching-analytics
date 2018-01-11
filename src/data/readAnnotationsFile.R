require(XML)

# Function to read the ELAN video coding annotations into a data frame
# Assumes layers: Comments, Recording markers, Activity, Social plane, Experimental part
readAnnotationsFile <- function(file){
    
    
    # Parse the XML
    rawXML <- xmlParse(file)
    parsedXML <- xmlToList(rawXML)
    # Extract the time markers for the annotations, to be used later
    annotationTimes <- data.frame(matrix(unlist(parsedXML[["TIME_ORDER"]]), nrow=length(parsedXML[["TIME_ORDER"]]), byrow=T))
    names(annotationTimes) <- c("TIME_SLOT_ID", "TIME_VALUE")
    
    # We get the data for each Annotation Tier
    comments <- data.frame(start=numeric(), end=numeric(), annotation=character())
    social <- data.frame(start=numeric(), end=numeric(), annotation=character())
    # experimental <- data.frame(start=numeric(), end=numeric(), annotation=character())
    activity <- data.frame(start=numeric(), end=numeric(), annotation=character())
    recording <- data.frame(start=numeric(), end=numeric(), annotation=character())
    for (tier in parsedXML[names(parsedXML) %in% "TIER"]){
        if(class(tier) != "list") next; #If it is an empty tier, it will not be a list, we just pass
        
        tierName <- tier$.attrs["TIER_ID"]
        
        if(tierName=="Comments"){# The Comments tier
            for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                annot <- annot2$ALIGNABLE_ANNOTATION
                annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                         end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                         annotation=annot$ANNOTATION_VALUE)
                comments <- rbind(comments,annotation)
            }
        }
        
        if(tierName=="Recording markers"){# Recording markers tier
            for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                annot <- annot2$ALIGNABLE_ANNOTATION
                annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                         end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                         annotation=annot$ANNOTATION_VALUE)
                recording <- rbind(recording,annotation)
            }
        }
        
        if(tierName=="Activity"){# Activities tier
            for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                annot <- annot2$ALIGNABLE_ANNOTATION
                annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                         end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                         annotation=annot$ANNOTATION_VALUE)
                activity <- rbind(activity,annotation)
            }
        }
        
        if(tierName=="Social plane"){# social plane tier
            for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                annot <- annot2$ALIGNABLE_ANNOTATION
                annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                         end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                         annotation=annot$ANNOTATION_VALUE)
                social <- rbind(social,annotation)
            }
        }
        
        # if(tierName=="Experimental part"){# Experimental part tier
        #     for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
        #         annot <- annot2$ALIGNABLE_ANNOTATION
        #         annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
        #                                  end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
        #                                  annotation=annot$ANNOTATION_VALUE)
        #         experimental <- rbind(experimental,annotation)
        #     }
        # }
        
    }
    
    #Add tier field to all data frames, and put them together, and add the session field too
    comments$tier = "Comments"
    recording$tier = "Recording"
    activity$tier = "Activity"
    social$tier = "Social"
#    experimental$tier = "Experimental"
#    annotations = rbind(comments,recording,activity,social,experimental)
    annotations = rbind(comments,recording,activity,social)
    annotations

    
}