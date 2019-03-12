!>@file   t_boundary_field_IO.f90
!!@brief  module t_boundary_field_IO
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Array for boundary condition data IO
!!
!!@verbatim
!!      subroutine alloc_num_bc_values
!!      subroutine alloc_boundary_values
!!
!!      subroutine dealloc_boundary_values
!!
!!      subroutine read_bc_condition_file                               &
!!     &         (id_rank, nod_grp, sf_grp, IO_bc)
!!      subroutine write_boundary_values_file(id_rank, IO_bc)
!!        type(IO_boundary), intent(inout) :: IO_bc
!!
!!    format for data file for boundary data
!!
!!      line   :  number of type of boundary
!!      line...:  name of group type (node or surface) and group name
!!      line   :  name of physical values
!!               ( temperature, heat_flux,
!!                 velocity_x, velocity_y, velocity_z,
!!                 torque_x, torque_y, torque_z, normal_velocity
!!                 pressure,
!!                 magnetic_x, magne_y, magne_z,
!!                 magnetic_potential, infinity
!!                 dunny_scalar )
!!      line...:   values
!!@endverbatim
      module t_boundary_field_IO
!
      use m_precision
!
      implicit  none
!
      character(len=kchara),parameter :: flag_nod_grp=  'Node_group'
!
      character(len=kchara),parameter :: flag_surf_grp= 'Surface_group'
!
      type IO_boundary
!>   number of boundary data from data file
        integer(kind=kint) :: num_group
        character(len=kchara), allocatable :: group_type(:)
!>  group name for boundary data
        character(len=kchara), allocatable :: group_name(:)
!>
!  data type of boundary data
        character(len=kchara), allocatable :: field_type(:)
!
!>  stack ID of boundary data from data file
        integer(kind=kint), allocatable ::  istack_data(:)
!
!>   total number of boundary data from data file
        integer(kind=kint) :: ntot_field
        integer(kind=kint), allocatable :: id_local_fld(:)
!  values of boundary data from data file
        real(kind=kreal), allocatable :: d_field(:)
      end type IO_boundary
!
      character(len=kchara) :: boundary_data_head = 'boundary'
!
      integer(kind=kint), parameter :: boundary_data_code = 12
!
      private :: boundary_data_code
      private :: read_boundary_values_file
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_bc_condition_file                                 &
     &         (id_rank, nod_grp, sf_grp, IO_bc)
!
      use t_group_data
!
      type(group_data), intent(in) :: nod_grp
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: id_rank
!
      type(IO_boundary), intent(inout) :: IO_bc
!
!
      call read_boundary_values_file(id_rank,                           &
     &    nod_grp%num_grp, nod_grp%istack_grp, nod_grp%grp_name,        &
     &    sf_grp%num_grp, sf_grp%istack_grp, sf_grp%grp_name, IO_bc)
!
      end subroutine read_bc_condition_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_bc_values(IO_bc)
!
      type(IO_boundary), intent(inout) :: IO_bc
!
!
      allocate( IO_bc%group_name(IO_bc%num_group) )
      allocate( IO_bc%group_type(IO_bc%num_group) )
      allocate( IO_bc%field_type(IO_bc%num_group) )
      allocate( IO_bc%istack_data(0:IO_bc%num_group) )
!
      IO_bc%istack_data = 0
!
      end subroutine alloc_num_bc_values
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_boundary_values(IO_bc)
!
      type(IO_boundary), intent(inout) :: IO_bc
!
!
      allocate( IO_bc%id_local_fld(IO_bc%ntot_field) )
      allocate( IO_bc%d_field(IO_bc%ntot_field) )
!
      if(IO_bc%ntot_field .le. 0) return
      IO_bc%id_local_fld = 0
      IO_bc%d_field = 0.0d0
!
      end subroutine alloc_boundary_values
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_boundary_values(IO_bc)
!
      type(IO_boundary), intent(inout) :: IO_bc
!
!
      deallocate(IO_bc%d_field,  IO_bc%id_local_fld)
!
      deallocate( IO_bc%group_name, IO_bc%group_type)
      deallocate( IO_bc%field_type  )
      deallocate( IO_bc%istack_data )
!
      end subroutine dealloc_boundary_values
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_boundary_values_file(id_rank,                     &
     &          num_bc, bc_istack, bc_name,                             &
     &          num_surf, surf_istack, surf_name, IO_bc)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: id_rank
!
      integer(kind = kint), intent(in) :: num_bc
      integer(kind = kint), intent(in) :: bc_istack(0:num_bc)
      character(len=kchara), intent(in) :: bc_name(num_bc)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      type(IO_boundary), intent(inout) :: IO_bc
!
      character(len=kchara) :: bc_file_name
      integer (kind=kint) :: i, j, ist, jed
!
!
      bc_file_name = add_int_suffix(id_rank, boundary_data_head)
      open (boundary_data_code, file=bc_file_name)
!
      read(boundary_data_code,*) IO_bc%num_group
!      write(*,*) 'IO_bc%num_group',IO_bc%num_group
!
      call alloc_num_bc_values(IO_bc)
!
      do i = 1, IO_bc%num_group
        read(boundary_data_code,*)  IO_bc%group_type(i),                &
     &                              IO_bc%group_name(i)
      end do
!
!      do i = 1, IO_bc%num_group
!       write(*,*) 'IO_bc%group_name', i, IO_bc%group_name(i)
!      end do
!     count number of data
!
      do i = 1, IO_bc%num_group
        if(IO_bc%group_type(i) .eq. flag_nod_grp) then
          do j = 1, num_bc
            if ( IO_bc%group_name(i) .eq. bc_name(j) ) then
              IO_bc%istack_data(i) = IO_bc%istack_data(i-1)             &
     &                              + bc_istack(j) - bc_istack(j-1)
              exit
            end if
          end do
        end if
!
        if(IO_bc%group_type(i) .eq. flag_surf_grp) then
          do j = 1, num_surf
            if ( IO_bc%group_name(i) .eq. surf_name(j) ) then
              IO_bc%istack_data(i) = IO_bc%istack_data(i-1)             &
     &                              + surf_istack(j) - surf_istack(j-1)
              exit
            end if
          end do
        end if
      end do
      IO_bc%ntot_field = IO_bc%istack_data(IO_bc%num_group)
!
      call alloc_boundary_values(IO_bc)
!
!      do i = 0, IO_bc%num_group
!       write(*,*) 'IO_bc%istack_data', i, IO_bc%istack_data(i)
!      end do
!  ---------   read data
!
      do i = 1, IO_bc%num_group
        read(boundary_data_code,*) IO_bc%field_type(i)
!       write(*,*) IO_bc%field_type(i)
!
        ist = IO_bc%istack_data(i-1) + 1
        jed = IO_bc%istack_data(i)
        do j = ist, jed
          read(boundary_data_code,*) IO_bc%id_local_fld(j),             &
     &                              IO_bc%d_field(j)
        end do
!       do j = jstart, jed
!         write(*,*) IO_bc%d_field(j)
!       end do
      end do
!
      close (boundary_data_code)
!
      end subroutine read_boundary_values_file
!
!-----------------------------------------------------------------------
!
      subroutine write_boundary_values_file(id_rank, IO_bc)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: id_rank
      type(IO_boundary), intent(inout) :: IO_bc
!
      character(len=kchara) :: bc_file_name
      integer (kind=kint) :: i, j, jst, jed
!
!
      bc_file_name = add_int_suffix(id_rank, boundary_data_head)
      open (boundary_data_code, file=bc_file_name)
!
      write(boundary_data_code,'(i16)') IO_bc%num_group
!
      do i = 1, IO_bc%num_group
        write(boundary_data_code,'(a,a2,a)')                            &
     &      trim(IO_bc%group_type(i)),', ', trim(IO_bc%group_name(i))
      end do
!
!  ---------   write data
!
      do i = 1, IO_bc%num_group
        write(boundary_data_code,'(a)') trim(IO_bc%field_type(i))
!
        jst = IO_bc%istack_data(i-1) + 1
        jed = IO_bc%istack_data(i)
        do j = jst, jed
          write(boundary_data_code,'(i16,1pe23.14e3)')                  &
     &              IO_bc%id_local_fld(j), IO_bc%d_field(j)
        end do
      end do
!
      close(boundary_data_code)
      call dealloc_boundary_values(IO_bc)
!
      end subroutine write_boundary_values_file
!
!-----------------------------------------------------------------------
!
      end module t_boundary_field_IO
