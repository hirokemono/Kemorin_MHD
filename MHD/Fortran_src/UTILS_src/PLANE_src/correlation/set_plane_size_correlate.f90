!
!      module set_plane_size_correlate
!
!     Written by H. Matsui
!
!!      subroutine s_set_plane_size_correlate                           &
!!     &         (cube_c, c_size, num_pe, num_pe2)
!!        type(size_of_cube), intent(inout) :: c_size
!!       subroutine check_dominsize_data_2(cube_c)
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
     &         (cube_c, c_size, num_pe, num_pe2)
!
      use t_size_of_cube
      use m_ctl_data_2nd_plane
      use m_correlate_4_plane
!
      type(ctl_data_4_plane_model), intent(in) :: cube_c
!
      type(size_of_cube), intent(inout) :: c_size
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
      num_pe2 =  ndomain_plane2_ctl%intvalue(1)                         &
     &         * ndomain_plane2_ctl%intvalue(2)                         &
     &         * ndomain_plane2_ctl%intvalue(3)
!
       end subroutine s_set_plane_size_correlate
!
!  ---------------------------------------------------------------------
!
       subroutine check_dominsize_data_2(cube_c)
!
      use m_ctl_data_2nd_plane
!
      type(ctl_data_4_plane_model), intent(in) :: cube_c
!
      integer:: isig
!
      isig = 0
!
      if (   plane_size2_ctl%realvalue(1)                               &
     &        .ne. cube_c%plane_size_ctl%realvalue(1)                   &
     &  .or. plane_size2_ctl%realvalue(2)                               &
     &        .ne. cube_c%plane_size_ctl%realvalue(2)                   &
     &  .or. plane_size2_ctl%realvalue(3)                               &
     &        .ne. cube_c%plane_size_ctl%realvalue(3)                   &
     &       ) isig = 1
      if (   unit_len_plane2_ctl%charavalue(1)                          &
     &        .ne. cube_c%unit_len_plane_ctl%charavalue(1)              &
     &  .or. unit_len_plane2_ctl%charavalue(2)                          &
     &        .ne. cube_c%unit_len_plane_ctl%charavalue(2)              &
     &  .or. unit_len_plane2_ctl%charavalue(3)                          &
     &        .ne. cube_c%unit_len_plane_ctl%charavalue(3)              &
     &      ) isig = 1
      if (   nnod_plane2_ctl%intvalue(1)                                &
     &        .ne. cube_c%nnod_plane_ctl%intvalue(1)                    &
     &  .or. nnod_plane2_ctl%intvalue(2)                                &
     &        .ne. cube_c%nnod_plane_ctl%intvalue(2)                    &
     &  .or. nnod_plane2_ctl%intvalue(3)                                &
     &        .ne. cube_c%nnod_plane_ctl%intvalue(3)                    &
     &       ) isig = 1
      if (horizontal_grid2_ctl%charavalue                               &
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
