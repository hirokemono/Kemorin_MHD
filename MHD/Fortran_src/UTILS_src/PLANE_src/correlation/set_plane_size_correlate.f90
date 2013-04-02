!
!      module set_plane_size_correlate
!
      module set_plane_size_correlate
!
!     Written by H. Matsui
!
      use m_precision
!
      implicit    none
!
!       subroutine s_set_plane_size_correlate(num_pe2)
!       subroutine check_dominsize_data_2
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
      nx_all = nnod_plane_ctl(1)
      ny_all = nnod_plane_ctl(2)
      nz_all = nnod_plane_ctl(3)
      kx_max = nnod_plane_ctl(1)
      ky_max = nnod_plane_ctl(2)
      iz_max = nnod_plane_ctl(3)
      num_domain =  nnod_plane_ctl(1)                                   &
     &            * nnod_plane_ctl(2)                                   &
     &            * nnod_plane_ctl(3)
       num_pe =   ndomain_plane_ctl(1)                                  &
     &          * ndomain_plane_ctl(2)                                  &
     &          * ndomain_plane_ctl(3)
!
!
      num_pe2 =  ndomain_plane2_ctl(1)                                  &
     &         * ndomain_plane2_ctl(2)                                  &
     &         * ndomain_plane2_ctl(3)
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
      if (   plane_size2_ctl(1) .ne. plane_size_ctl(1)                 &
     &  .or. plane_size2_ctl(2) .ne. plane_size_ctl(2)                 &
     &  .or. plane_size2_ctl(3) .ne. plane_size_ctl(3)                 &
     &       ) isig = 1
      if (   unit_len_plane2_ctl(1) .ne. unit_len_plane_ctl(1)         &
     &  .or. unit_len_plane2_ctl(2) .ne. unit_len_plane_ctl(2)         &
     &  .or. unit_len_plane2_ctl(3) .ne. unit_len_plane_ctl(3)         &
     &      ) isig = 1
      if (   nnod_plane2_ctl(1) .ne. nnod_plane_ctl(1)                 &
     &  .or. nnod_plane2_ctl(2) .ne. nnod_plane_ctl(2)                 &
     &  .or. nnod_plane2_ctl(3) .ne. nnod_plane_ctl(3)                 &
     &       ) isig = 1
      if ( horizontal_grid2_ctl .ne. horizontal_grid_ctl ) isig = 1
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
