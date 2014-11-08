!>@file   boundary_field_file_IO.f90
!!@brief  module boundary_field_file_IO
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2000
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!
!> @brief IO routine for boundary fields
!!
!!@verbatim
!!      subroutine read_boundary_values_file(my_rank,                   &
!!     &          num_bc, bc_istack, bc_name,                           &
!!     &          num_surf, surf_istack, surf_name)
!!      subroutine write_boundary_values_file(my_rank)
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
!!
!!@endverbatim
!
      module boundary_field_file_IO
!
      use m_precision
!
      implicit none
!
      character(len=kchara) :: boundary_data_head = 'boundary'
!
      character(len=kchara) :: bc_file_name
      integer(kind=kint), parameter :: boundary_data_code = 12
!
      private :: boundary_data_code, bc_file_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_boundary_values_file(my_rank,                     &
     &          num_bc, bc_istack, bc_name,                             &
     &          num_surf, surf_istack, surf_name)
!
      use m_boundary_field_IO
      use set_parallel_file_name
!
      integer (kind=kint), intent(in) :: my_rank
!
      integer(kind = kint), intent(in) :: num_bc
      integer(kind = kint), intent(in) :: bc_istack(0:num_bc)
      character(len=kchara), intent(in) :: bc_name(num_bc)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: i, j, ist, jed
!
!
      call add_int_suffix(my_rank, boundary_data_head, bc_file_name)
      open (boundary_data_code, file=bc_file_name)
!
      read(boundary_data_code,*) num_bc_group_IO
!      write(*,*) 'num_bc_group_IO',num_bc_group_IO
!
      call allocate_num_bc_values
!
      do i = 1, num_bc_group_IO
        read(boundary_data_code,*)  bc_group_type_IO(i),                &
     &                              bc_data_group_IO(i)
      end do
!
!      do i = 1, num_bc_group_IO
!       write(*,*) 'bc_data_group_IO', i, bc_data_group_IO(i)
!      end do
!     count number of data
!
      do i = 1, num_bc_group_IO
        if(bc_group_type_IO(i) .eq. flag_nod_grp) then
          do j = 1, num_bc
            if ( bc_data_group_IO(i) .eq. bc_name(j) ) then
              istack_bc_data_IO(i) = istack_bc_data_IO(i-1)             &
     &                              + bc_istack(j) - bc_istack(j-1)
              exit
            end if
          end do
        end if
!
        if(bc_group_type_IO(i) .eq. flag_surf_grp) then
          do j = 1, num_surf
            if ( bc_data_group_IO(i) .eq. surf_name(j) ) then
              istack_bc_data_IO(i) = istack_bc_data_IO(i-1)             &
     &                              + surf_istack(j) - surf_istack(j-1)
              exit
            end if
          end do
        end if
      end do
      ntot_boundary_field_IO = istack_bc_data_IO(num_bc_group_IO)
!
      call allocate_boundary_values
!
!      do i = 0, num_bc_group_IO
!       write(*,*) 'istack_bc_data_IO', i, istack_bc_data_IO(i)
!      end do
!  ---------   read data
!
      do i = 1, num_bc_group_IO
        read(boundary_data_code,*) bc_field_type_IO(i)
!       write(*,*) bc_field_type_IO(i)
!
        ist = istack_bc_data_IO(i-1) + 1
        jed = istack_bc_data_IO(i)
        do j = ist, jed
          read(boundary_data_code,*) id_local_bc_fld_IO(j),             &
     &                              boundary_field_IO(j)
        end do
!       do j = jstart, jed
!         write(*,*) boundary_field_IO(j)
!       end do
      end do
!
      close (boundary_data_code)
!
      end subroutine read_boundary_values_file
!
!-----------------------------------------------------------------------
!
      subroutine write_boundary_values_file(my_rank)
!
      use m_boundary_field_IO
      use set_parallel_file_name
!
      integer (kind=kint), intent(in) :: my_rank
!
      integer (kind=kint) :: i, j, jst, jed
!
!
      call add_int_suffix(my_rank, boundary_data_head, bc_file_name)
      open (boundary_data_code, file=bc_file_name)
!
      write(boundary_data_code,'(i15)') num_bc_group_IO
!
      do i = 1, num_bc_group_IO
        write(boundary_data_code,'(a,a2,a)')                            &
     &      trim(bc_group_type_IO(i)),', ', trim(bc_data_group_IO(i))
      end do
!
!  ---------   write data
!
      do i = 1, num_bc_group_IO
        write(boundary_data_code,'(a)') trim(bc_field_type_IO(i))
!
        jst = istack_bc_data_IO(i-1) + 1
        jed = istack_bc_data_IO(i)
        do j = jst, jed
          write(boundary_data_code,'(i15,1pe23.14e3)')                  &
     &              id_local_bc_fld_IO(j), boundary_field_IO(j)
        end do
      end do
!
      close (boundary_data_code)
      call deallocate_boundary_values
!
      end subroutine write_boundary_values_file
!
!-----------------------------------------------------------------------
!
      end module boundary_field_file_IO
