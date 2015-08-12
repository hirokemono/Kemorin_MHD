!
!   module m_int_vol_data
!.......................................................................
!
!      subroutine allocate_int_vol_data(numele)
!      subroutine check_diff_elemental_data                             &
!     &         (my_rank, numele, numdir, i_field)
!
      module m_int_vol_data
!
      use m_precision
!
      implicit  none
!
!
      real (kind=kreal), allocatable ::  vect_e(:,:)
      real (kind=kreal), allocatable ::  velo_1(:,:)
      real (kind=kreal), allocatable ::  magne_1(:,:)
      real (kind=kreal), allocatable ::  vect_1(:,:)
      real (kind=kreal), allocatable ::  tensor_e(:,:)
! 
      real (kind=kreal), allocatable :: xe(:,:)
      real (kind=kreal), allocatable :: radius_e(:)
!
      real (kind=kreal), allocatable  :: phi_e(:)
      real (kind=kreal), allocatable  :: temp_e(:)
      real (kind=kreal), allocatable  :: d_scalar_e(:)
!
      real (kind=kreal), allocatable  :: sgs_e(:,:)
      real (kind=kreal), allocatable  :: sgs_t(:,:)
!
!
      real (kind=kreal), allocatable :: dvx(:,:)
! 
      integer(kind=kint) :: num_dvxi
      integer(kind=kint) :: i_dvx = 0, i_dfvx = 0
      integer(kind=kint) :: i_dtx = 0, i_dftx = 0
      integer(kind=kint) :: i_dcx = 0, i_dfcx = 0
      integer(kind=kint) :: i_dbx = 0, i_dfbx = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_int_vol_data(numele)
!
      use m_control_parameter
      use m_phys_labels
      use m_node_phys_data
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint) :: i
!
!
      allocate(xe(numele,3))
      allocate(radius_e(numele))
      allocate(phi_e(numele))
      allocate(vect_e(numele,3))
      allocate(tensor_e(numele,6))
!
      if(numele .gt. 0) then
        xe = 0.0d0
        radius_e = 0.0d0
        phi_e = 0.0d0
        tensor_e = 0.0d0
        vect_e = 0.0d0
      end if
!
!
       do i = 1, num_nod_phys
        if      ( phys_nod_name(i) .eq. fhd_velo ) then
          allocate(velo_1(numele,3))
          if(numele .gt. 0) velo_1 = 0.0d0
        else if ( phys_nod_name(i) .eq. fhd_temp ) then
          allocate(temp_e(numele))
          if(numele .gt. 0) temp_e = 0.0d0
        else if ( phys_nod_name(i) .eq. fhd_light ) then
          allocate(d_scalar_e(numele))
          if(numele .gt. 0) d_scalar_e = 0.0d0
        else if ( phys_nod_name(i) .eq. fhd_magne ) then
          allocate(magne_1(numele,3))
          if(numele .gt. 0) magne_1 = 0.0d0
        else if ( phys_nod_name(i) .eq. fhd_vecp ) then
          allocate(vect_1(numele,3))
          if(numele .gt. 0) vect_1 = 0.0d0
        end if
       end do
!
       if (iflag_SGS_model .ne. id_SGS_none) then
          allocate(sgs_e(numele,3))
          allocate(sgs_t(numele,6))
          if(numele .gt. 0) sgs_t = 0.0d0
          if(numele .gt. 0) sgs_e = 0.0d0
       end if
!
       num_dvxi = 0
       if ( iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none) then
         num_dvxi = num_dvxi + 18
        end if
!
!        if (iflag_SGS_heat .ne. id_SGS_none) then
!         num_dvxi = num_dvxi + 6
!        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         num_dvxi = num_dvxi + 18
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         num_dvxi = num_dvxi + 18
        end if
!
       else if (iflag_SGS_model .ne. id_SGS_none) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none ) then
         num_dvxi = num_dvxi + 9
        end if
!
!        if (iflag_SGS_heat .ne. id_SGS_none) then
!         num_dvxi = num_dvxi + 3
!        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         num_dvxi = num_dvxi + 9
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         num_dvxi = num_dvxi + 9
        end if
!
       end if
!
       allocate(dvx(numele,num_dvxi))
       if(numele .gt. 0) dvx = 0.0d0
!
       i = 1
       if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none ) then
         i_dvx = i
         i_dfvx = i + 9
         i = i + 18
        end if
!
!        if ( iflag_SGS_heat .ne. id_SGS_none) then
!         i_dtx = i
!         i_dftx = i + 3
!         i = i + 6
!        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         i_dbx = i
         i_dfbx = i + 9
         i = i + 18
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         i_dbx = i
         i_dfbx = i + 9
         i = i + 18
        end if
!
       else if (iflag_SGS_model .ne. id_SGS_none                        &
     &      .and. iflag_dynamic_SGS .eq. id_SGS_DYNAMIC_OFF) then
        if (   iflag_SGS_heat .ne.     id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none) then
         i_dvx = i
         i = i + 9
        end if
!
!        if ( iflag_SGS_heat .ne. id_SGS_none) then
!         i_dtx = i
!         i = i + 3
!        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         i_dbx = i
         i = i + 9
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         i_dbx = i
         i = i + 9
        end if
!
       end if
!
       end subroutine allocate_int_vol_data
!
!  ------------------------------------------------------------------
!  ------------------------------------------------------------------
!
      subroutine check_diff_elemental_data                              &
     &         (my_rank, numele, numdir, i_field)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: numele, numdir, i_field
      integer(kind = kint) :: iele, nd, ndiff
!
      do nd = 1, numdir
        write(50+my_rank,*)                                             &
     &      'iele, diff. of elemental field: ', i_field, nd
       do iele = 1, numele
        write(50+my_rank,'(i16,1p10e25.14)')                            &
     &         iele, (dvx(iele,i_field+3*(nd-1)+ndiff-1),ndiff=1, 3)
       end do
      end do
!
      end subroutine check_diff_elemental_data
!
!-----------------------------------------------------------------------
!
      end module   m_int_vol_data
