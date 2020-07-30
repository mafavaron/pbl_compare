! Evaluation of standard validation indicators on some types of differences along a transect.
!
! This program is part of the pbl-compare project, and is covered by the MIT license.
!
! Author: Patrizia Favaron

program GenerateIndicators

    use pbl_compare

    implicit none

    ! Locals
    integer                 :: iRetCode
    integer                 :: iVariant
    real                    :: rValue
    real, dimension(16384)  :: rvPrimarySet
    real, dimension(16384)  :: rvSecondarySet
    type(CompareType)       :: tCmp
    
    ! First case: Single peak of growing size
    rvPrimarySet   = 1.
    rvSecondarySet = rvPrimarySet
    
    do iVariant = 0, 6
    
        print *, "Action: 'Peaks'; Variant = ", iVariant
    
        rValue = iVariant * 100.
        rvSecondarySet(8192) = rValue
        
        iRetCode = tCmp % Set(rvPrimarySet, rvSecondarySet)
        if(iRetCode /= 0) then
            print *, "error: Comparison data sets initialization failure - Ret code = ", iRetCode
            stop
        end if
        
    end do

end program GenerateIndicators

