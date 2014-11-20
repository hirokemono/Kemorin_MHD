!write_line_filter_data.f90
!     module write_line_filter_data
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine write_line_filter_data_a(file_code, numnod)
!      subroutine write_line_filter_data_b(file_code, numnod)
!
      module write_line_filter_data
!
      use m_precision
!
      use m_l_filtering_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_line_filter_data_a(file_code, numnod)
!
      integer (kind = kint) :: file_code, numnod
      integer (kind = kint) :: nd, i, ifil
!
!
       write(file_code,'(a)')                                           &
     &        '! num_depth, max. number of node for filtering cube: '
       write(file_code,'(2i16)') ndepth_l, num_filter_l
       write(file_code,'(a)')                                           &
     &     '! num_depth, max. and min. number of node for filtering: '
       do i = 1, 3
        write(file_code,'(3i16)') i, nmax_l_filter(i), nmin_l_filter(i)
       end do
       write(file_code,'(a)') '! total number of filtering data '
       write(file_code,'(3i16)') (num_l_filter(i),i=1,3)
!
!
       write(file_code,'(a)') '! orderind ID for filtering'
       write(file_code,'(a)')                                           &
     &     '! and stack for filtering for each direction'
       do i = 1, numnod
          write(file_code,'(6i16)') (inod_l_filter(i,nd), nd=1, 3),     &
      &                          (istack_l_filter(i,nd), nd=1, 3)
       enddo
!
          write(file_code,'(a)') '! node ID for x-filtering'
          write(file_code,'(8i16)')                                     &
     &           (item_l_filter(i,1),i = 1, num_l_filter(1))
          write(file_code,'(a)') '! node ID for y-filtering'
          write(file_code,'(8i16)')                                     &
     &           (item_l_filter(i,2),i = 1, num_l_filter(2))
          write(file_code,'(a)') '! node ID for z-filtering'
          write(file_code,'(8i16)')                                     &
     &           (item_l_filter(i,3),i = 1, num_l_filter(3))
!
          write(file_code,'(a)')                                        &
     &          '! filter coefficients for x'
          write(file_code,'(1p4E25.15e3)')                              &
     &               (coef_l_filter(i,1),i = 1, num_l_filter(1))
          write(file_code,'(a)')                                        &
     &          '! filter coefficients for y'
          write(file_code,'(1p4E25.15e3)')                              &
     &               (coef_l_filter(i,2),i = 1, num_l_filter(2))
          write(file_code,'(a)')                                        &
     &          '! filter coefficients for z'
          write(file_code,'(1p4E25.15e3)')                              &
     &               (coef_l_filter(i,3),i = 1, num_l_filter(3))
!
!
      end subroutine write_line_filter_data_a
!
! ----------------------------------------------------------------------
!
      subroutine write_line_filter_data_b(file_code, numnod)
!
      integer (kind = kint) :: file_code, numnod
!
      integer (kind = kint) :: nd, i, ifil
!
!
!       write(file_code)                                                    &
!     &        '! num_depth, max. number of node for filtering cube: '
       write(file_code) ndepth_l, num_filter_l
!       write(file_code)                                                     &
!     &        '! num_depth, max. and min. number of node for filtering: '
        write(file_code) (nmax_l_filter(nd),nd=1,3)
        write(file_code) (nmin_l_filter(nd),nd=1,3)
!
!       write(file_code) '! total number of filtering data '
       write(file_code) (num_l_filter(nd),nd=1,3)
!
!       write(file_code) '! orderind ID for filtering'
         write(file_code)  ((inod_l_filter(i,nd),i=1,numnod), nd=1,3)
!
!       write(file_code) '! stack for filtering for each direction'
         write(file_code)  ((istack_l_filter(i,nd),i=1,numnod), nd=1,3)
!
!       write(file_code) '!  node ID for filtering
          write(file_code)                                              &
     &       ((item_l_filter(i,nd),i=1,ntot_l_filter), nd=1,3)
!       write(file_code) '!  filter coefficients'
          write(file_code)                                              &
     &               ((coef_l_filter(i,nd),i=1,ntot_l_filter),nd=1,3)
!
!
      end subroutine write_line_filter_data_b
!
! ----------------------------------------------------------------------
!
      end module write_line_filter_data
