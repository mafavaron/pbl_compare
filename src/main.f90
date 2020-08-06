! Evaluation of standard validation indicators on some types of differences along a transect.
!
! This program is part of the pbl-compare project, and is covered by the MIT license.
!
! Author: Patrizia Favaron

program GenerateIndicators

    use pbl_compare

    implicit none
    
    ! Constants
    integer, parameter  :: N_VARS   =     7
    integer, parameter  :: N_CASES  =     4
    integer, parameter  :: N        = 16384

    ! Locals
    integer                             :: iRetCode
    integer                             :: iVariant
    integer                             :: iCase
    integer                             :: iShiftVal
    real                                :: rValue
    real, dimension(N, N_CASES)         :: rmPrimarySet
    real, dimension(N, N_CASES)         :: rmSecondarySet
    real, dimension(N_VARS, N_CASES)    :: rmFB
    type(CompareType)                   :: tCmp
    real                                :: rFB
    real                                :: rDs
    integer                             :: i
    
    print *, "   "
    print *, "---   ---   ---"
    print *, "   "
    
    print *, "Peak ---------------------------------------------------"
        
    do iVariant = 0, N_VARS - 1
    
        do iCase = 1, N_CASES
    
            select case(iCase)
            
            case(1)
            
                ! First case: Single peak of growing size
                rmPrimarySet(:,iCase)   = 1.
                rmSecondarySet(:,iCase) = rmPrimarySet(:,iCase)
                if(iVariant > 0) then
                    rValue = (iVariant + 1) * 100.
                    rmSecondarySet(8192,iCase)  = rValue
                    rDs                         = rValue
                else
                    rDs = 1.
                end if
                
            case(2)
            
                ! Second case: Heaviside doublets
                rmPrimarySet(:,iCase)   = Heaviside(N, floor(N/4) - floor(N/8) - 1) - &
                                          Heaviside(N, floor(N/4) + floor(N/8) - 1)
                rmSecondarySet(:,iCase) = Heaviside(N, FLOOR(N/2) + floor(N/4) - floor(N/8) - 1) - &
                                          Heaviside(N, FLOOR(N/2) + floor(N/4) + floor(N/8) - 1)
            
            end select
            
        
        iRetCode = tCmp % Set(rvPrimarySet, rvSecondarySet)
        if(iRetCode /= 0) then
            print *, "error: Comparison data sets initialization failure - Ret code = ", iRetCode
            stop
        end if
        
        ! Compute value expected analytically, for comparison
        rValue = 2. * (1. - rDs) / (rDs + 2*n - 1.)
        
        rFB = tCmp % FB()
        print *, "    Fractional Bias (FB) = ", rFB, "    Error = ", rFB - rValue
        
    end do
    
    print *, "Norm ---------------------------------------------------"
    
    open(10, file="FB.csv", status="unknown", action="write")
    rvPrimarySet = [(exp(-(i-n/2)**2/2.e4),i=1,n)]
    do iShiftVal = -6000, 6000, 100
        rvSecondarySet = [(exp(-(i-n/2+iShiftVal)**2/2.e4),i=1,n)]
        iRetCode = tCmp % Set(rvPrimarySet, rvSecondarySet)
        rFB = tCmp % FB()
        print *, "    Fractional Bias (FB) = ", rFB, "  Shift = ", iShiftVal
    end do
    close(10)

    print *, "==== ---------------------------------------------------"
        
end program GenerateIndicators

