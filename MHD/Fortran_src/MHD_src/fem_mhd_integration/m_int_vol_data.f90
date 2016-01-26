!
!   module m_int_vol_data
!.......................................................................
!
!      subroutine allocate_int_vol_data(numele, max_nod_smp)
!      subroutine check_diff_elemental_data                             &
!     &         (my_rank, numele, numdir, i_field)
!
      module m_int_vol_data
!
      use m_precision
      use t_MHD_finite_element_mat
!
      implicit  none
!
!
!>      Work array for FEM assemble in MHD model
      type(work_MHD_fe_mat), save :: mhd_fem1_wk
! 
      integer(kind=kint) :: i_dvx = 0, i_dfvx = 0
      integer(kind=kint) :: i_dtx = 0, i_dftx = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_int_vol_data(numele, max_nod_smp)
!
      use m_control_parameter
      use m_phys_labels
      use m_node_phys_data
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numele, max_nod_smp
      integer(kind = kint) :: i
!
!
      allocate(mhd_fem1_wk%ff_m_smp(max_nod_smp,3,np_smp))
      allocate(mhd_fem1_wk%ff_t_smp(max_nod_smp,6,np_smp))
      if(max_nod_smp .gt. 0) mhd_fem1_wk%ff_m_smp = 0.0d0
      if(max_nod_smp .gt. 0) mhd_fem1_wk%ff_t_smp = 0.0d0
!
      allocate(mhd_fem1_wk%xx_e(numele,3))
      allocate(mhd_fem1_wk%rr_e(numele))
!
      if(numele .gt. 0) then
        mhd_fem1_wk%xx_e = 0.0d0
        mhd_fem1_wk%rr_e = 0.0d0
      end if
!
!
       do i = 1, nod_fld1%num_phys
        if      ( nod_fld1%phys_name(i) .eq. fhd_velo ) then
          allocate(mhd_fem1_wk%velo_1(numele,3))
          if(numele .gt. 0) mhd_fem1_wk%velo_1 = 0.0d0
        else if ( nod_fld1%phys_name(i) .eq. fhd_magne ) then
          allocate(mhd_fem1_wk%magne_1(numele,3))
          if(numele .gt. 0) mhd_fem1_wk%magne_1 = 0.0d0
        else if ( nod_fld1%phys_name(i) .eq. fhd_vecp ) then
          allocate(mhd_fem1_wk%vecp_1(numele,3))
          if(numele .gt. 0) mhd_fem1_wk%vecp_1 = 0.0d0
        end if
       end do
!
       if (iflag_SGS_model .ne. id_SGS_none) then
          allocate(mhd_fem1_wk%sgs_v1(numele,3))
          allocate(mhd_fem1_wk%sgs_t1(numele,6))
          if(numele .gt. 0) mhd_fem1_wk%sgs_v1 = 0.0d0
          if(numele .gt. 0) mhd_fem1_wk%sgs_t1 = 0.0d0
       end if
!
       mhd_fem1_wk%n_dvx = 0
       if ( iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none) then
         mhd_fem1_wk%n_dvx = mhd_fem1_wk%n_dvx + 18
        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         mhd_fem1_wk%n_dvx = mhd_fem1_wk%n_dvx + 18
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         mhd_fem1_wk%n_dvx = mhd_fem1_wk%n_dvx + 18
        end if
!
       else if (iflag_SGS_model .ne. id_SGS_none) then
        if (  iflag_SGS_heat .ne.      id_SGS_none                      &
     &   .or. iflag_SGS_inertia .ne.   id_SGS_none                      &
     &   .or. iflag_SGS_induction .ne. id_SGS_none ) then
         mhd_fem1_wk%n_dvx = mhd_fem1_wk%n_dvx + 9
        end if
!
        if ( iflag_SGS_lorentz .ne. id_SGS_none) then
         mhd_fem1_wk%n_dvx = mhd_fem1_wk%n_dvx + 9
        else if (iflag_SGS_induction .ne. id_SGS_none                   &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         mhd_fem1_wk%n_dvx = mhd_fem1_wk%n_dvx + 9
        end if
!
       end if
!
       allocate(mhd_fem1_wk%dvx(numele,mhd_fem1_wk%n_dvx))
       if(numele .gt. 0) mhd_fem1_wk%dvx = 0.0d0
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
        write(50+my_rank,'(i16,1p10e25.14)') iele,                      &
     &    (mhd_fem1_wk%dvx(iele,i_field+3*(nd-1)+ndiff-1),ndiff=1, 3)
       end do
      end do
!
      end subroutine check_diff_elemental_data
!
!-----------------------------------------------------------------------
!
      end module   m_int_vol_data
