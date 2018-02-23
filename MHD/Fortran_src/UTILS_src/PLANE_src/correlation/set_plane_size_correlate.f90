!
!      module set_plane_size_correlate
!
!     Written by H. Matsui
!
!       subroutine s_set_plane_size_correlate(num_pe2)
!       subroutine check_dominsize_data_2
!
      module set_plane_size_correlate
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_plane_size_correlate(num_pe, num_pe2)
!
      use m_ctl_data_4_plane_model
      use m_ctl_data_2nd_plane
      use m_correlate_4_plane
      use m_size_4_plane
!
!     read outline of mesh
!
       integer (kind=kint), intent(inout) :: num_pe, num_pe2
!
!
      nx_all = nnod_plane_ctl%intvalue(1)
      ny_all = nnod_plane_ctl%intvalue(2)
      nz_all = nnod_plane_ctl%intvalue(3)
      kx_max = nx_all
      ky_max = ny_all
      iz_max = nz_all
      num_domain =  nx_all * ny_all * nz_all
       num_pe =   ndomain_plane_ctl%intvalue(1)                         &
     &          * ndomain_plane_ctl%intvalue(2)                         &
     &          * ndomain_plane_ctl%intvalue(3)
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
       subroutine check_dominsize_data_2
!
!     write outline of mesh
!
      use m_ctl_data_4_plane_model
      use m_ctl_data_2nd_plane
!
      integer:: isig
!
      isig = 0
!
      if (   plane_size2_ctl%realvalue(1)                               &
     &        .ne. plane_size_ctl%realvalue(1)                          &
     &  .or. plane_size2_ctl%realvalue(2)                               &
     &        .ne. plane_size_ctl%realvalue(2)                          &
     &  .or. plane_size2_ctl%realvalue(3)                               &
     &        .ne. plane_size_ctl%realvalue(3)                          &
     &       ) isig = 1
      if (   unit_len_plane2_ctl%charavalue(1)                          &
     &        .ne. unit_len_plane_ctl%charavalue(1)                     &
     &  .or. unit_len_plane2_ctl%charavalue(2)                          &
     &        .ne. unit_len_plane_ctl%charavalue(2)                     &
     &  .or. unit_len_plane2_ctl%charavalue(3)                          &
     &        .ne. unit_len_plane_ctl%charavalue(3)                     &
     &      ) isig = 1
      if (   nnod_plane2_ctl%intvalue(1)                                &
     &        .ne. nnod_plane_ctl%intvalue(1)                           &
     &  .or. nnod_plane2_ctl%intvalue(2)                                &
     &        .ne. nnod_plane_ctl%intvalue(2)                           &
     &  .or. nnod_plane2_ctl%intvalue(3)                                &
     &        .ne. nnod_plane_ctl%intvalue(3)                           &
     &       ) isig = 1
      if (horizontal_grid2_ctl%charavalue                               &
     &        .ne. horizontal_grid_ctl%charavalue) isig = 1
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
