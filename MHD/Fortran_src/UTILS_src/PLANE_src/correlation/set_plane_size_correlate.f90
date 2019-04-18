!
!      module set_plane_size_correlate
!
!     Written by H. Matsui
!
!!      subroutine s_set_plane_size_correlate                           &
!!     &         (cube_c, cube2nd_c, c_size, num_pe, num_pe2,           &
!!     &          kx_max, ky_max, iz_max, num_domain_c)
!!       subroutine check_dominsize_data_2(cube_c, cube2nd_c)
!!        type(size_of_cube), intent(inout) :: c_size, cube2nd_c
!
      module set_plane_size_correlate
!
      use m_precision
      use t_ctl_data_4_plane_model
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_plane_size_correlate                             &
     &         (cube_c, cube2nd_c, c_size, num_pe, num_pe2,             &
     &          kx_max, ky_max, iz_max, num_domain_c)
!
      use t_size_of_cube
!
      type(ctl_data_4_plane_model), intent(in) :: cube_c, cube2nd_c
!
      type(size_of_cube), intent(inout) :: c_size
      integer(kind=kint ), intent(inout) :: kx_max, ky_max
      integer(kind=kint ), intent(inout) :: iz_max, num_domain_c
      integer , intent(inout) :: num_pe, num_pe2
!
!
      c_size%nx_all = cube_c%nnod_plane_ctl%intvalue(1)
      c_size%ny_all = cube_c%nnod_plane_ctl%intvalue(2)
      c_size%nz_all = cube_c%nnod_plane_ctl%intvalue(3)

      kx_max = c_size%nx_all
      ky_max = c_size%ny_all
      iz_max = c_size%nz_all
      num_domain_c =  c_size%nx_all * c_size%ny_all * c_size%nz_all
       num_pe =   cube_c%ndomain_plane_ctl%intvalue(1)                  &
     &          * cube_c%ndomain_plane_ctl%intvalue(2)                  &
     &          * cube_c%ndomain_plane_ctl%intvalue(3)
!
!
      num_pe2 =  cube2nd_c%ndomain_plane_ctl%intvalue(1)                &
     &         * cube2nd_c%ndomain_plane_ctl%intvalue(2)                &
     &         * cube2nd_c%ndomain_plane_ctl%intvalue(3)
!
       end subroutine s_set_plane_size_correlate
!
!  ---------------------------------------------------------------------
!
       subroutine check_dominsize_data_2(cube_c, cube2nd_c)
!
      type(ctl_data_4_plane_model), intent(in) :: cube_c, cube2nd_c
!
      integer:: isig
!
      isig = 0
!
      if (   cube2nd_c%plane_size_ctl%realvalue(1)                      &
     &        .ne. cube_c%plane_size_ctl%realvalue(1)                   &
     &  .or. cube2nd_c%plane_size_ctl%realvalue(2)                      &
     &        .ne. cube_c%plane_size_ctl%realvalue(2)                   &
     &  .or. cube2nd_c%plane_size_ctl%realvalue(3)                      &
     &        .ne. cube_c%plane_size_ctl%realvalue(3)                   &
     &       ) isig = 1
      if (   cube2nd_c%unit_len_plane_ctl%charavalue(1)                 &
     &        .ne. cube_c%unit_len_plane_ctl%charavalue(1)              &
     &  .or. cube2nd_c%unit_len_plane_ctl%charavalue(2)                 &
     &        .ne. cube_c%unit_len_plane_ctl%charavalue(2)              &
     &  .or. cube2nd_c%unit_len_plane_ctl%charavalue(3)                 &
     &        .ne. cube_c%unit_len_plane_ctl%charavalue(3)              &
     &      ) isig = 1
      if (   cube2nd_c%nnod_plane_ctl%intvalue(1)                       &
     &        .ne. cube_c%nnod_plane_ctl%intvalue(1)                    &
     &  .or. cube2nd_c%nnod_plane_ctl%intvalue(2)                       &
     &        .ne. cube_c%nnod_plane_ctl%intvalue(2)                    &
     &  .or. cube2nd_c%nnod_plane_ctl%intvalue(3)                       &
     &        .ne. cube_c%nnod_plane_ctl%intvalue(3)                    &
     &       ) isig = 1
      if (cube2nd_c%horizontal_grid_ctl%charavalue                      &
     &        .ne. cube_c%horizontal_grid_ctl%charavalue) isig = 1
!
      if (isig .eq. 1)  then
        write(*,*) 'these data have different resolution!!'
        stop
      end if
!
      return
!
       end subroutine check_dominsize_data_2
!
!  ---------------------------------------------------------------------
!
      end module set_plane_size_correlate
