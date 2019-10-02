!> @file  set_sph_boundary_from_file.f90
!!      module set_sph_boundary_from_file
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine set_bc_for_sph_scalar_by_file(sph_rj,                &
!!     &          num_bc_mode, imode_gl, bc_input, bc_data)
!!      subroutine set_bc_for_sph_vector_by_file(sph_rj,                &
!!     &          num_bc_mode, imode_gl, bc_input,                      &
!!     &          vp_data, dp_data, vt_data)
!!
!!      subroutine set_bc_for_evolved_sph_by_file(sph_rj,               &
!!     &          num_bc_mode, imode_gl, bc_input, bc_data)
!!      subroutine bc_for_evo_vect2_sph_by_file(sph_rj,                 &
!!     &          num_bc_mode, imode_gl, bc_input, vp_data, vt_data)
!!      subroutine bc_for_evo_vector_sph_by_file(sph_rj,                &
!!     &          num_bc_mode, imode_gl, bc_input,                      &
!!     &          vp_data, dp_data, vt_data)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      integer(kind = kint) function num_comp_bc_data(label)
!!@endverbatim
!
      module set_sph_boundary_from_file
!
      use m_precision
      use m_phys_labels
      use t_spheric_rj_data
!
      implicit  none
!
!>      Postfix for magnetide
      character(len=kchara), parameter :: mag_label =   '_magnitude'
!>      Postfix for frequency
      character(len=kchara), parameter :: freq1_label = '_freq'
!>      Postfix for frequency
      character(len=kchara), parameter :: freq2_label = '_freqency'
!>      Postfix for initial phase
      character(len=kchara), parameter :: phase_label = '_phase'
!
!
      private :: find_scalar_bc_label, find_vector_bc_label
      private :: find_bc_label
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_for_sph_scalar_by_file(sph_rj,                  &
     &          num_bc_mode, imode_gl, bc_input, bc_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: num_bc_mode
      integer(kind = kint), intent(in) :: imode_gl(2,num_bc_mode)
      real(kind = kreal), intent(in) :: bc_input(num_bc_mode,1)
!
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, num_bc_mode
        l = int(imode_gl(1,inum))
        m = int(imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) bc_data(j) = bc_input(inum,1)
      end do
!
      end subroutine set_bc_for_sph_scalar_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_for_sph_vector_by_file(sph_rj,                  &
     &          num_bc_mode, imode_gl, bc_input,                        &
     &          vp_data, dp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: num_bc_mode
      integer(kind = kint), intent(in) :: imode_gl(2,num_bc_mode)
      real(kind = kreal), intent(in) :: bc_input(num_bc_mode,3)
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: dp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, num_bc_mode
        l = int(imode_gl(1,inum))
        m = int(imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_input(inum,1)
          dp_data(j) = bc_input(inum,2)
          vt_data(j) = bc_input(inum,3)
        end if
      end do
!
      end subroutine set_bc_for_sph_vector_by_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bc_for_evo_scalar_sph_by_file(sph_rj,                  &
     &          num_bc_mode, ncomp_bc, imode_gl, bc_input, bc_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: num_bc_mode, ncomp_bc
      integer(kind = kint), intent(in) :: imode_gl(2,num_bc_mode)
      real(kind = kreal), intent(in) :: bc_input(num_bc_mode,ncomp_bc)
!
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, num_bc_mode
        l = int(imode_gl(1,inum))
        m = int(imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) bc_data(j) = bc_input(inum,1)
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) bc_data(j) = bc_input(inum,1)
      end do
!
      end subroutine bc_for_evo_scalar_sph_by_file
!
! -----------------------------------------------------------------------
!
      subroutine bc_for_evo_vect2_sph_by_file(sph_rj,                   &
     &          num_bc_mode, imode_gl, bc_input, vp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: num_bc_mode
      integer(kind = kint), intent(in) :: imode_gl(2,num_bc_mode)
      real(kind = kreal), intent(in) :: bc_input(num_bc_mode,2)
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, num_bc_mode
        l = int(imode_gl(1,inum))
        m = int(imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_input(inum,1)
          vt_data(j) = bc_input(inum,2)
        end if
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) then
          vp_data(j) = bc_input(inum,1)
          vt_data(j) = bc_input(inum,2)
        end if
      end do
!
      end subroutine bc_for_evo_vect2_sph_by_file
!
! -----------------------------------------------------------------------
!
      subroutine bc_for_evo_vector_sph_by_file(sph_rj,                  &
     &          num_bc_mode, imode_gl, bc_input,                        &
     &          vp_data, dp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: num_bc_mode
      integer(kind = kint), intent(in) :: imode_gl(2,num_bc_mode)
      real(kind = kreal), intent(in) :: bc_input(num_bc_mode,3)
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: dp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, num_bc_mode
        l = int(imode_gl(1,inum))
        m = int(imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_input(inum,1)
          dp_data(j) = bc_input(inum,2)
          vt_data(j) = bc_input(inum,3)
        end if
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) then
          vp_data(j) = bc_input(inum,1)
          dp_data(j) = bc_input(inum,2)
          vt_data(j) = bc_input(inum,3)
        end if
      end do
!
      end subroutine bc_for_evo_vector_sph_by_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function num_comp_bc_data(label)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: num_comp = 0
!
      if(      cmp_no_case(label, fhd_velo)                             &
     &    .or. cmp_no_case(label, fhd_vort)                             &
     &    .or. cmp_no_case(label, fhd_magne)) num_comp = 3
!
      if(      cmp_no_case(label, fhd_temp)                             &
     &    .or. cmp_no_case(label, fhd_light)                            &
     &    .or. cmp_no_case(label, fhd_entropy)                          &
     &    .or. cmp_no_case(label, fhd_h_flux)                           &
     &    .or. cmp_no_case(label, fhd_c_flux)) num_comp = 1
!
      if(find_vector_bc_label(label, mag_label))   num_comp = 3
      if(find_vector_bc_label(label, phase_label)) num_comp = 3
      if(find_vector_bc_label(label, freq1_label)) num_comp = 2
      if(find_vector_bc_label(label, freq2_label)) num_comp = 2
!
      if(find_scalar_bc_label(label, mag_label))   num_comp = 1
      if(find_scalar_bc_label(label, phase_label)) num_comp = 1
      if(find_scalar_bc_label(label, freq1_label)) num_comp = 1
      if(find_scalar_bc_label(label, freq2_label)) num_comp = 1
!
       num_comp_bc_data = num_comp
!
      end function num_comp_bc_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function find_scalar_bc_label(label, postfix)
!
      character(len = kchara), intent(in) :: label, postfix
!
      find_scalar_bc_label = find_bc_label(label, fhd_temp, postfix)    &
     &                  .or. find_bc_label(label, fhd_light, postfix)   &
     &                  .or. find_bc_label(label, fhd_entropy, postfix) &
     &                  .or. find_bc_label(label, fhd_h_flux, postfix)  &
     &                  .or. find_bc_label(label, fhd_c_flux, postfix)
!
      end function find_scalar_bc_label
!
! -----------------------------------------------------------------------
!
      logical function find_vector_bc_label(label, postfix)
!
      character(len = kchara), intent(in) :: label, postfix
!
      find_vector_bc_label  = find_bc_label(label, fhd_velo, postfix)   &
     &                   .or. find_bc_label(label, fhd_vort, postfix)   &
     &                   .or. find_bc_label(label, fhd_magne, postfix)
!
      end function find_vector_bc_label
!
! -----------------------------------------------------------------------
!
      logical function find_bc_label(label, field_name, postfix)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: label, field_name, postfix
!
      character(len = kchara) :: tmpchara
!
      write(tmpchara,'(a,a)') trim(field_name), trim(postfix)
      find_bc_label = cmp_no_case(label, tmpchara)
!
      end function find_bc_label
!
! -----------------------------------------------------------------------
!
      end module set_sph_boundary_from_file
