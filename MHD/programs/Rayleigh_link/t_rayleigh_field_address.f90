!>@file   t_rayleigh_field_address.f90
!!@brief  module t_rayleigh_field_address
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine set_ctl_rayleigh_field_address                       &
!!     &         (plt, field_ctl, rayleigh_ftbl, e_msg, ierr)
!!      subroutine dealloc_rayleigh_field_address(rayleigh_ftbl)
!!      subroutine check_rayleigh_field_address(rayleigh_ftbl)
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_control), intent(in) :: field_ctl
!!        type(rayleigh_field_address), intent(inout) :: rayleigh_ftbl
!!      subroutine init_fields_by_rayleigh(rayleigh_ftbl, mesh, field)
!!        type(rayleigh_field_address), intent(in) :: rayleigh_ftbl
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data), intent(inout) :: field
!!      subroutine init_fields_IO_by_rayleigh                           &
!!     &         (rayleigh_ftbl, mesh_rayleigh, fld_IO)
!!        type(rayleigh_field_address), intent(in) :: rayleigh_ftbl
!!        type(mesh_geometry), intent(in) :: mesh_rayleigh
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module t_rayleigh_field_address
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use calypso_mpi
!
      implicit  none
!
!>      Structure for Rayleigh field address
      type rayleigh_field_address
!>        Directory name
        character(len = kchara) :: field_dir
!
!>        Number of fields
        integer(kind = kint) :: num_field = 0
!>        Total number of components
        integer(kind = kint) :: ntot_comp = 0
!>        Number of components
        integer(kind = kint), allocatable :: num_comp(:)
!>        Stack of number of components
        integer(kind = kint), allocatable :: istack_comp(:)
!>        field name
        character(len = kchara), allocatable :: field_name(:)
!>        component address in rayleigh
        integer(kind = kint), allocatable :: id_rayleigh(:)
      end type rayleigh_field_address
!
      private :: alloc_rayleigh_field_num, alloc_rayleigh_field_address
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_ctl_rayleigh_field_address                         &
     &         (plt, field_ctl, rayleigh_ftbl, e_msg, ierr)
!
      use m_error_IDs
      use t_ctl_data_4_fields
      use t_ctl_data_4_platforms
!
      type(platform_data_control), intent(in) :: plt
      type(field_control), intent(in) :: field_ctl
      type(rayleigh_field_address), intent(inout) :: rayleigh_ftbl
!
      character(len = kchara), intent(inout) :: e_msg
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: num_field, i, inum, icou
!
!
      ierr = 0
      if(plt%rayleigh_field_dir%iflag .eq. 0) then
        e_msg = 'Set directory for Rayleigh field data'
        ierr = ierr_file
        return
      else
        rayleigh_ftbl%field_dir = plt%rayleigh_field_dir%charavalue
      end if
!
      num_field                                                         &
     &    = field_ctl%scalar_phys%num + field_ctl%vector_phys%num
      call alloc_rayleigh_field_num(num_field, rayleigh_ftbl)
!
      if(rayleigh_ftbl%num_field .le. 0) then
        e_msg = 'Set field list for Rayleigh'
        ierr = ierr_fld
        return
      end if
!
      do i = 1, field_ctl%scalar_phys%num
        rayleigh_ftbl%field_name(i) =  field_ctl%scalar_phys%c_tbl(i)
        rayleigh_ftbl%num_comp(i) = 1
      end do
      do inum = 1, field_ctl%vector_phys%num
        i = inum + field_ctl%scalar_phys%num
        rayleigh_ftbl%field_name(i) = field_ctl%vector_phys%c_tbl(inum)
        rayleigh_ftbl%num_comp(i) = 3
      end do
!
      rayleigh_ftbl%istack_comp(0) = 0
      do i = 1, rayleigh_ftbl%num_field
        rayleigh_ftbl%istack_comp(i) = rayleigh_ftbl%istack_comp(i-1)   &
     &                                + rayleigh_ftbl%num_comp(i)
      end do
      rayleigh_ftbl%ntot_comp                                           &
     &        = rayleigh_ftbl%istack_comp(rayleigh_ftbl%num_field)
!
      call alloc_rayleigh_field_address(rayleigh_ftbl)
!
      do inum = 1, field_ctl%scalar_phys%num
        i = rayleigh_ftbl%istack_comp(inum-1)
        rayleigh_ftbl%id_rayleigh(i+1)                                  &
     &       = field_ctl%scalar_phys%ivec(inum)
      end do
      do inum = 1, field_ctl%vector_phys%num
        icou = inum + field_ctl%scalar_phys%num
        i = rayleigh_ftbl%istack_comp(icou-1)
        rayleigh_ftbl%id_rayleigh(i+1)                                  &
     &       = field_ctl%vector_phys%ivec1(inum)
        rayleigh_ftbl%id_rayleigh(i+2)                                  &
     &       = field_ctl%vector_phys%ivec2(inum)
        rayleigh_ftbl%id_rayleigh(i+3)                                  &
     &       = field_ctl%vector_phys%ivec3(inum)
      end do
!
      end subroutine set_ctl_rayleigh_field_address
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_field_address(rayleigh_ftbl)
!
      type(rayleigh_field_address), intent(inout) :: rayleigh_ftbl
!
!
      deallocate(rayleigh_ftbl%field_name)
      deallocate(rayleigh_ftbl%num_comp, rayleigh_ftbl%istack_comp)
      deallocate(rayleigh_ftbl%id_rayleigh)
!
      end subroutine dealloc_rayleigh_field_address
!
!-----------------------------------------------------------------------
!
      subroutine check_rayleigh_field_address(rayleigh_ftbl)
!
      type(rayleigh_field_address), intent(in) :: rayleigh_ftbl
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(*,*) 'rayleigh_ftbl%num_field', rayleigh_ftbl%num_field
      do i = 1, rayleigh_ftbl%num_field
        ist = rayleigh_ftbl%istack_comp(i-1) + 1
        ied = rayleigh_ftbl%istack_comp(i)
        write(*,*) i, rayleigh_ftbl%num_comp(i),                        &
     &            trim(rayleigh_ftbl%field_name(i)), ':  ',             &
     &            rayleigh_ftbl%id_rayleigh(ist:ied)
      end do
!
      end subroutine check_rayleigh_field_address
!
!-----------------------------------------------------------------------
!
      subroutine init_fields_by_rayleigh(rayleigh_ftbl, mesh, field)
!
      use t_mesh_data
      use t_phys_data
      use share_field_data
!
      type(rayleigh_field_address), intent(in) :: rayleigh_ftbl
      type(mesh_geometry), intent(in) :: mesh
!
      type(phys_data), intent(inout) :: field
!
!
      if(my_rank .eq. 0) then
        field%num_phys = rayleigh_ftbl%num_field
        call alloc_phys_name_type(field)
!
        field%phys_name(1:field%num_phys)                               &
     &          = rayleigh_ftbl%field_name(1:field%num_phys)
        field%istack_component(0:field%num_phys)                        &
     &          = rayleigh_ftbl%istack_comp(0:field%num_phys)
        field%num_component(1:field%num_phys)                           &
     &          = rayleigh_ftbl%num_comp(1:field%num_phys)
        field%ntot_phys = rayleigh_ftbl%ntot_comp
      end if
!
      call share_phys_field_names(field)
      field%num_phys_viz =  field%num_phys
      field%ntot_phys_viz = field%ntot_phys
!
      call alloc_phys_data_type(mesh%node%numnod, field)
!
      end subroutine init_fields_by_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine init_fields_IO_by_rayleigh                             &
     &         (rayleigh_ftbl, mesh_rayleigh, fld_IO)
!
      use t_mesh_data
      use t_field_data_IO
!
      type(rayleigh_field_address), intent(in) :: rayleigh_ftbl
      type(mesh_geometry), intent(in) :: mesh_rayleigh
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      fld_IO%nnod_IO = mesh_rayleigh%node%numnod
      fld_IO%num_field_IO = rayleigh_ftbl%num_field
      call alloc_phys_name_IO(fld_IO)
!
      fld_IO%fld_name(1:fld_IO%num_field_IO)                            &
     &      = rayleigh_ftbl%field_name(1:fld_IO%num_field_IO)
      fld_IO%num_comp_IO(1:fld_IO%num_field_IO)                         &
     &      = rayleigh_ftbl%num_comp(1:fld_IO%num_field_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      end subroutine init_fields_IO_by_rayleigh
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_field_num(num_field, rayleigh_ftbl)
!
      integer(kind = kint), intent(in) :: num_field
      type(rayleigh_field_address), intent(inout) :: rayleigh_ftbl
!
!
      rayleigh_ftbl%num_field = num_field
      allocate(rayleigh_ftbl%field_name(rayleigh_ftbl%num_field))
      allocate(rayleigh_ftbl%num_comp(rayleigh_ftbl%num_field))
      allocate(rayleigh_ftbl%istack_comp(0:rayleigh_ftbl%num_field))
!
      rayleigh_ftbl%num_comp =    0
      rayleigh_ftbl%istack_comp = 0
!
      end subroutine alloc_rayleigh_field_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_field_address(rayleigh_ftbl)
!
      type(rayleigh_field_address), intent(inout) :: rayleigh_ftbl
!
!
      allocate(rayleigh_ftbl%id_rayleigh(rayleigh_ftbl%ntot_comp))
!
      rayleigh_ftbl%id_rayleigh = 0
!
      end subroutine alloc_rayleigh_field_address
!
!-----------------------------------------------------------------------
!
      end module t_rayleigh_field_address
