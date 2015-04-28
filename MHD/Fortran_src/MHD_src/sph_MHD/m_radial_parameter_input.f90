!> @file  m_radial_parameter_input.f90
!!      module m_radial_parameter_input
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine deallocalte_r_params_ctl
!!
!!      subroutine read_radial_parameter_file
!!      subroutine write_radial_parameter_file
!!
!!      subroutine set_radial_parameter_by_file(field_name, nri,        &
!!     &          ref_grp, radius_data, iflag_bc_scalar)
!!
!!  ---------------------------------------------------------------------
!!      Data format
!!       line 1:  Number of total boundary conditions to be defined
!!
!!       line 2:     Field name to define the first boundary condition
!!       line 3:     Number of spherical harmonics modes 
!!                    for each boundary condition
!!       line 4...:  Spectrum data for the boundary conditions 
!!                  (degree $l$, order $m$, and harmonics coefficients)
!!        Return to 2...
!!  ---------------------------------------------------------------------
!!@endverbatim
!!
!!@param    field_name       Field name to be define
!!                           the boundary condition by file
!!@param    ref_grp          Boundary group name to be defined
!!                           the boundary condition by file
!!@param    jamx             Number of local spherical harmonics modes
!!@param    bc_data(jmax)    Local boundary condition spectrum
!!@param    iflag_bc_scalar  Boundary condition type flag
!
      module m_radial_parameter_input
!
      use m_precision
!
      implicit  none
!
!>      File name for radial parameter file
      character(len=kchara) :: bc_sph_file_name = 'radial_parameter.dat'
!>      File ID for radial parameter file
      integer(kind = kint), parameter :: id_radial_file = 41
!
!>        Structure for each radial parameter
      type each_radial_parameter
!>        Name of radial parameter field
        character(len=kchara) :: r_param_name
!>        Number of components for radial parameter
        integer(kind = kint) :: ncomp_r_param
!>        Number of radial points for radial parameter
        integer(kind = kint) :: nri_param
!>        Radial ID for radial parameter
        integer(kind = kint), pointer :: kr_param(:)
!>        boundary condition spectrum  r_param(r_ID,component)
        real(kind = kreal), pointer ::   r_param(:,:)
      end type each_radial_parameter
!
!>        Number of radial parameter
      integer(kind = kint), save :: num_r_param_ctl
!>        Structures for boundary conditions
      type(each_radial_parameter), allocatable, save :: rfld_ctls(:)
!
      private :: id_radial_file
!
      private :: alloc_each_r_param_ctl, dealloc_each_r_param_ctl
      private :: allocalte_r_params_ctl, set_num_comp_r_params
      private :: set_radius_params_by_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_each_r_param_ctl(rfld)
!
      type(each_radial_parameter), intent(inout) :: rfld
!
!
      allocate(rfld%kr_param(rfld%nri_param))
      allocate(rfld%r_param(rfld%nri_param,rfld%ncomp_r_param))
!
      if(rfld%nri_param .gt. 0) then
        rfld%kr_param = 0
        rfld%r_param =  0.0d0
      end if
!
      end subroutine alloc_each_r_param_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_r_param_ctl(rfld)
!
      type(each_radial_parameter), intent(inout) :: rfld
!
!
      deallocate(rfld%kr_param, rfld%r_param)
!
      end subroutine dealloc_each_r_param_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocalte_r_params_ctl
!
!
      allocate(rfld_ctls(num_r_param_ctl))
      rfld_ctls(1:num_r_param_ctl)%nri_param = 0
      rfld_ctls(1:num_r_param_ctl)%ncomp_r_param =    1
!
      end subroutine allocalte_r_params_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocalte_r_params_ctl
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_r_param_ctl
        call dealloc_each_r_param_ctl(rfld_ctls(i))
      end do
      deallocate(rfld_ctls)
!
      end subroutine deallocalte_r_params_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_radial_parameter_file
!
      use m_machine_parameter
      use skip_comment_f
!
      integer(kind = kint) :: igrp, inum, num
      character(len=255) :: tmpchara
!
!
      if(iflag_debug .gt. 0) write(*,*) 'read boundary condition: ',    &
     &                      trim(bc_sph_file_name)
      open(id_radial_file, file=bc_sph_file_name)
!
      call skip_comment(tmpchara,id_radial_file)
      read(tmpchara,*) num_r_param_ctl
!
      call allocalte_r_params_ctl
!
      do igrp = 1, num_r_param_ctl
        call skip_comment(tmpchara,id_radial_file)
        read(tmpchara,*)  rfld_ctls(igrp)%r_param_name
        call skip_comment(tmpchara,id_radial_file)
        read(tmpchara,*) rfld_ctls(igrp)%nri_param
!
        call set_num_comp_r_params                                      &
     &    (rfld_ctls(igrp)%r_param_name, rfld_ctls(igrp)%ncomp_r_param)
        call alloc_each_r_param_ctl(rfld_ctls(igrp))
!
        do inum = 1, rfld_ctls(igrp)%nri_param
          num = rfld_ctls(igrp)%ncomp_r_param
          call skip_comment(tmpchara,id_radial_file)
          read(tmpchara,*) rfld_ctls(igrp)%kr_param(inum),              &
     &                     rfld_ctls(igrp)%r_param(inum,1:num)
        end do
      end do
      close(id_radial_file)
!
      end subroutine read_radial_parameter_file
!
! -----------------------------------------------------------------------
!
      subroutine write_radial_parameter_file
!
      integer(kind = kint) :: igrp, inum
!
!
      open(id_radial_file, file=bc_sph_file_name)
!
      write(id_radial_file,'(a)') '#'
      write(id_radial_file,'(a)') '#  number of radial parameter'
      write(id_radial_file,'(a)') '#'
!
      write(id_radial_file,'(i16)') num_r_param_ctl
!
      do igrp = 1, num_r_param_ctl
        write(id_radial_file,'(a)') '#'
        write(id_radial_file,'(a)') '#   radial parameter list'
        write(id_radial_file,'(a)') '#'
!
        write(id_radial_file,'(a)') trim(rfld_ctls(igrp)%r_param_name)
        write(id_radial_file,'(i16)') rfld_ctls(igrp)%nri_param
!
        do inum = 1, rfld_ctls(igrp)%nri_param
          write(id_radial_file,'(2i16,1p10E25.15e3)')                   &
     &       rfld_ctls(igrp)%kr_param(inum),                              &
     &       rfld_ctls(igrp)%r_param(inum,1:rfld_ctls(igrp)%ncomp_r_param)
        end do
      end do
!
      close(id_radial_file)
!
      end subroutine write_radial_parameter_file
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_parameter_by_file(field_name, nri,          &
     &          radius_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
      use skip_comment_f
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(inout) :: radius_data(nri)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_scalar .ne. iflag_undefined_bc) return
      do igrp = 1, num_r_param_ctl
        if(cmp_no_case(rfld_ctls(igrp)%r_param_name, field_name) ) then
          iflag_bc_scalar =  iflag_fixed_field
          call set_radius_params_by_file(rfld_ctls(igrp),               &
     &        nri, radius_data)
        end if
      end do
!
      end subroutine set_radial_parameter_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_radius_params_by_file(rfld, nri, radius_data)
!
      use m_spheric_parameter
!
      type(each_radial_parameter), intent(in) :: rfld
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(inout) :: radius_data(nri)
!
      integer(kind = kint) :: inum
      integer(kind = 4) :: k
!
!
      do inum = 1, rfld%nri_param
        k = int(rfld%kr_param(inum))
        radius_data(k) = rfld%r_param(inum,1)
      end do
!
      end subroutine set_radius_params_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_num_comp_r_params(field_name, ncomp)
!
      use m_phys_labels
      use skip_comment_f
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(inout) :: ncomp
!
!
      if(    cmp_no_case(field_name, fhd_K_viscosity)            &
     &  .or. cmp_no_case(field_name, fhd_T_diffusivity)          &
     &  .or. cmp_no_case(field_name, fhd_C_diffusivity)          &
     &  .or. cmp_no_case(field_name, fhd_B_diffusivity)          &
     &  .or. cmp_no_case(field_name, fhd_ref_density))           &
     &      ncomp = 3
!
      if(    cmp_no_case(field_name, fhd_ref_temp))              &
     &      ncomp = 1
!
      end subroutine set_num_comp_r_params
!
! -----------------------------------------------------------------------
!
      end module m_radial_parameter_input
