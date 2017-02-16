!> @file  t_radial_parameter_input.f90
!!      module t_radial_parameter_input
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine deallocalte_r_params_ctl(r_file)
!!
!!      subroutine read_radial_parameter_file(r_file)
!!      subroutine write_radial_parameter_file(r_file)
!!
!!      subroutine set_radial_parameter_by_file(r_file, field_name, nri,&
!!     &          radius_data, iflag_bc_scalar)
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
      module t_radial_parameter_input
!
      use m_precision
!
      implicit  none
!
!>      File ID for radial parameter file
      integer(kind = kint), parameter :: id_radial_file = 41
!
!>      ID not to read external boundary condition file
      integer (kind=kint), parameter :: id_no_radial_file =   0
!>      ID to read external boundary condition file
      integer (kind=kint), parameter :: id_read_radial_file = 1
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
        integer(kind = kint), allocatable :: kr_param(:)
!>        boundary condition spectrum  r_param(r_ID,component)
        real(kind = kreal), allocatable ::   r_param(:,:)
      end type each_radial_parameter
!
!>        Structure for each radial parameter file IO
      type radial_parameter_file
!>        Flag to check external file for radial field
        integer (kind=kint) :: iflag_radial_param_file                  &
     &                                 = id_no_radial_file
!>        File name for radial parameter file
        character(len=kchara)                                           &
     &          :: sph_radial_data_name = 'radial_parameter.dat'
!>        Number of radial parameter
!
        integer(kind = kint) :: num_r_param_ctl
!>        Structures for boundary conditions
        type(each_radial_parameter), allocatable :: rfld_ctls(:)
      end type radial_parameter_file
!
      private :: id_radial_file
!
      private :: alloc_each_r_param_ctl, dealloc_each_r_param_ctl
      private :: alloc_r_params_ctl, set_num_comp_r_params
      private :: set_radius_params_by_file
      private :: read_radial_parameter_data
      private :: write_radial_parameter_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine deallocalte_r_params_ctl(r_file)
!
      type(radial_parameter_file), intent(inout) :: r_file
!
      integer(kind = kint) :: i
!
!
      do i = 1, r_file%num_r_param_ctl
        call dealloc_each_r_param_ctl(r_file%rfld_ctls(i))
      end do
      deallocate(r_file%rfld_ctls)
!
      end subroutine deallocalte_r_params_ctl
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
      subroutine read_radial_parameter_file(r_file)
!
      use m_machine_parameter
      use skip_comment_f
!
      type(radial_parameter_file), intent(inout) :: r_file
!
      integer(kind = kint) :: igrp
      character(len=255) :: tmpchara
!
!
      if(iflag_debug .gt. 0) write(*,*) 'read radial paramter data: ',  &
     &                      trim(r_file%sph_radial_data_name)
      open(id_radial_file, file = r_file%sph_radial_data_name)
!
      call skip_comment(tmpchara,id_radial_file)
      read(tmpchara,*) r_file%num_r_param_ctl
!
      call alloc_r_params_ctl(r_file)
!
      do igrp = 1, r_file%num_r_param_ctl
        call read_radial_parameter_data(r_file%rfld_ctls(igrp))
      end do
      close(id_radial_file)
!
      end subroutine read_radial_parameter_file
!
! -----------------------------------------------------------------------
!
      subroutine write_radial_parameter_file(r_file)
!
      type(radial_parameter_file), intent(in) :: r_file
!
      integer(kind = kint) :: igrp
!
!
      open(id_radial_file, file = r_file%sph_radial_data_name)
!
      write(id_radial_file,'(a)') '#'
      write(id_radial_file,'(a)') '#  number of radial parameter'
      write(id_radial_file,'(a)') '#'
!
      write(id_radial_file,'(i16)') r_file%num_r_param_ctl
!
      do igrp = 1, r_file%num_r_param_ctl
        call write_radial_parameter_data(r_file%rfld_ctls(igrp))
      end do
!
      close(id_radial_file)
!
      end subroutine write_radial_parameter_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_radial_parameter_by_file(r_file, field_name, nri,  &
     &          radius_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
!
      type(radial_parameter_file), intent(in) :: r_file
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(inout) :: radius_data(nri)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_scalar .ne. iflag_undefined_bc) return
      do igrp = 1, r_file%num_r_param_ctl
        call set_radius_params_by_file(r_file%rfld_ctls(igrp),          &
     &      field_name, nri, radius_data, iflag_bc_scalar)
      end do
!
      end subroutine set_radial_parameter_by_file
!
! -----------------------------------------------------------------------
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
      subroutine alloc_r_params_ctl(r_file)
!
      type(radial_parameter_file), intent(inout) :: r_file
!
!
      allocate(r_file%rfld_ctls(r_file%num_r_param_ctl))
      r_file%rfld_ctls(1:r_file%num_r_param_ctl)%nri_param =     0
      r_file%rfld_ctls(1:r_file%num_r_param_ctl)%ncomp_r_param = 1
!
      end subroutine alloc_r_params_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_radial_parameter_data(rfld)
!
      use m_machine_parameter
      use skip_comment_f
!
      type(each_radial_parameter), intent(inout) :: rfld
!
      integer(kind = kint) :: inum
      character(len=255) :: tmpchara
!
!
      call skip_comment(tmpchara,id_radial_file)
      read(tmpchara,*)  rfld%r_param_name
      call skip_comment(tmpchara,id_radial_file)
      read(tmpchara,*) rfld%nri_param
!
      call set_num_comp_r_params                                        &
     &   (rfld%r_param_name, rfld%ncomp_r_param)
      call alloc_each_r_param_ctl(rfld)
!
      do inum = 1, rfld%nri_param
        call skip_comment(tmpchara,id_radial_file)
        read(tmpchara,*) rfld%kr_param(inum),                           &
     &                   rfld%r_param(inum,1:rfld%ncomp_r_param)
      end do
!
      end subroutine read_radial_parameter_data
!
! -----------------------------------------------------------------------
!
      subroutine write_radial_parameter_data(rfld)
!
      type(each_radial_parameter), intent(in) :: rfld
!
      integer(kind = kint) :: inum
!
!
      write(id_radial_file,'(a)') '#'
      write(id_radial_file,'(a)') '#   radial parameter list'
      write(id_radial_file,'(a)') '#'
!
      write(id_radial_file,'(a)') trim(rfld%r_param_name)
      write(id_radial_file,'(i16)') rfld%nri_param
!
      do inum = 1, rfld%nri_param
          write(id_radial_file,'(2i16,1p10E25.15e3)')                   &
     &     rfld%kr_param(inum), rfld%r_param(inum,1:rfld%ncomp_r_param)
      end do
!
      end subroutine write_radial_parameter_data
!
! -----------------------------------------------------------------------
!
      subroutine set_radius_params_by_file                              &
     &         (rfld, field_name, nri, radius_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
      use skip_comment_f
!
      type(each_radial_parameter), intent(in) :: rfld
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(inout) :: radius_data(nri)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: inum
      integer(kind = 4) :: k
!
!
      if(cmp_no_case(rfld%r_param_name, field_name)) then
        do inum = 1, rfld%nri_param
          k = int(rfld%kr_param(inum))
          radius_data(k) = rfld%r_param(inum,1)
        end do
        iflag_bc_scalar =  iflag_fixed_field
      end if
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
      if(    cmp_no_case(field_name, fhd_K_viscosity)                   &
     &  .or. cmp_no_case(field_name, fhd_T_diffusivity)                 &
     &  .or. cmp_no_case(field_name, fhd_C_diffusivity)                 &
     &  .or. cmp_no_case(field_name, fhd_B_diffusivity)                 &
     &  .or. cmp_no_case(field_name, fhd_ref_density))                  &
     &      ncomp = 3
!
      if(    cmp_no_case(field_name, fhd_ref_temp)                      &
     &  .or. cmp_no_case(field_name, fhd_ref_light))                    &
     &      ncomp = 1
!
      end subroutine set_num_comp_r_params
!
! -----------------------------------------------------------------------
!
      end module t_radial_parameter_input
