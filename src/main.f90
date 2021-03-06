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
    integer, parameter  :: N        =    64

    ! Locals
    integer                             :: iRetCode
    integer                             :: iVariant
    integer                             :: iCase
    real                                :: rValue
    real, dimension(N_VARS)             :: rvVariantValue
    real, dimension(N, N_CASES)         :: rmPrimarySet
    real, dimension(N, N_CASES)         :: rmSecondarySet
    real, dimension(N_VARS, N_CASES)    :: rmFB
    real, dimension(N_VARS, N_CASES)    :: rmNMSE
    real, dimension(N_VARS, N_CASES)    :: rmMG
    real, dimension(N_VARS, N_CASES)    :: rmVG
    real, dimension(N_VARS, N_CASES)    :: rmFAC2
    real, dimension(N_VARS, N_CASES)    :: rmNAD
    type(CompareType)                   :: tCmp
    real                                :: rDs
    real                                :: rH
    integer                             :: i
    
    do iVariant = 0, N_VARS - 1
    
        !do iCase = 1, N_CASES
        do iCase = 1, 4
    
            select case(iCase)
            
            case(1)
            
                ! First case: Single peak of growing size
                rmPrimarySet(:,iCase)   = 1.
                rmSecondarySet(:,iCase) = rmPrimarySet(:,iCase)
                if(iVariant > 0) then
                    rDs = (iVariant + 1) * 100.
                    rmSecondarySet(1, iCase)  = rDs
                else
                    rDs = 1.
                end if
                rvVariantValue(iVariant+1) = rDs
                
            case(2)
            
                ! Second case: Heaviside doublets
                if(iVariant > 0) then
                    rH = (iVariant + 1) * 100.
                else
                    rH = 1.
                end if
                rvVariantValue(iVariant+1) = rH
                rmPrimarySet(:,iCase)   = rH * Case2(N, N/4 - N/8 - 1, N/4 + N/8 - 1)
                rmSecondarySet(:,iCase) = rH * Case2(N, N/2 + N/4 - N/8 - 1, N/2 + N/4 + N/8 - 1)
                
            case(3)
            
                ! Third case: dilated Gaussian
                if(iVariant > 0) then
                    rDs = (iVariant + 1) * 100.
                else
                    rDs = 1.
                end if
                rvVariantValue(iVariant+1) = rDs
                rmPrimarySet(:,iCase)   = TinyGaussian(N)
                rmSecondarySet(:,iCase) = rDs * rmPrimarySet(:,iCase)
                
            case(4)
            
                ! Fourth case: translated Gaussian
                if(iVariant > 0) then
                    rDs = (iVariant + 1) * 100.
                else
                    rDs = 0.
                end if
                rvVariantValue(iVariant+1) = rDs
                rmPrimarySet(:,iCase)   = TinyGaussian(N)
                rmSecondarySet(:,iCase) = rmPrimarySet(:,iCase) + rDs
            
            end select
        
            ! Initialize new value, then compute indices based on it
            iRetCode = tCmp % Set(rmPrimarySet(:,iCase), rmSecondarySet(:,iCase))
            if(iRetCode /= 0) then
                print *, "error: Comparison data sets initialization failure - Ret code = ", iRetCode
                stop
            end if
            rmFB(iVariant+1, iCase)   = tCmp % FB()
            rmNMSE(iVariant+1, iCase) = tCmp % NMSE()
            rmMG(iVariant+1, iCase)   = tCmp % MG()
            rmVG(iVariant+1, iCase)   = tCmp % VG()
            rmFAC2(iVariant+1, iCase) = tCmp % FAC2()
            rmNAD(iVariant+1, iCase)  = tCmp % NAD()
        
        end do
    
    end do
    
    ! Print results for the various cases
    ! -1- Case 1
    open(10, file="Indices_1.csv", status="unknown", action="write")
    write(10, "('Variant, Value, FB, NMSE, MG, VG, FAC2, NAD')")
    do i=1,N_VARS
        write(10, "(i1,',',f5.1,6(',',f11.5))") &
            i-1, rvVariantValue(i), rmFB(i,1), rmNMSE(i,1), rmMG(i,1), rmVG(i,1), rmFAC2(i,1), rmNAD(i,1)
    end do
    close(10)
    ! -1- Case 2
    open(10, file="Indices_2.csv", status="unknown", action="write")
    write(10, "('Variant, Value, FB, NMSE, MG, VG, FAC2, NAD')")
    do i=1,N_VARS
        write(10, "(i1,',',f5.1,6(',',f11.5))") &
            i-1, rvVariantValue(i), rmFB(i,2), rmNMSE(i,2), rmMG(i,2), rmVG(i,2), rmFAC2(i,2), rmNAD(i,2)
    end do
    close(10)
    ! -1- Case 3
    open(10, file="Indices_3.csv", status="unknown", action="write")
    write(10, "('Variant, Value, FB, NMSE, MG, VG, FAC2, NAD')")
    do i=1,N_VARS
        write(10, "(i1,',',f5.1,6(',',f11.5))") &
            i-1, rvVariantValue(i), rmFB(i,3), rmNMSE(i,3), rmMG(i,3), rmVG(i,3), rmFAC2(i,3), rmNAD(i,3)
    end do
    close(10)
    ! -1- Case 4
    open(10, file="Indices_4.csv", status="unknown", action="write")
    write(10, "('Variant, Value, FB, NMSE, MG, VG, FAC2, NAD')")
    do i=1,N_VARS
        write(10, "(i1,',',f5.1,6(',',f11.5))") &
            i-1, rvVariantValue(i), rmFB(i,4), rmNMSE(i,4), rmMG(i,4), rmVG(i,4), rmFAC2(i,4), rmNAD(i,4)
    end do
    close(10)

    print *, '*** End Job ***'
    
contains

    function Case2(n, n_min, n_max) result(rvCase2)
    
        ! Routine arguments
        integer, intent(in)             :: n
        integer, intent(in)             :: n_min
        integer, intent(in)             :: n_max
        real, dimension(n)              :: rvCase2
        
        ! Locals
        integer :: i
        
        ! Generate the information desired
        rvCase2 = 0.
        do i = 1, n
            if(i >= n_min .and. i <= n_max) then
                rvCase2(i) = 1.
            end if
        end do
        
    end function Case2
    
    
    function TinyGaussian(n) result(rvGauss)

        ! Routine arguments
        integer, intent(in)             :: n
        real, dimension(n)              :: rvGauss
        
        ! Locals
        integer :: i
        
        ! Compute the information desired
        rvGauss = exp(-50.*[((i-n/2)**2,i=1,n)]/n**2)
        
    end function TinyGaussian
        
end program GenerateIndicators

