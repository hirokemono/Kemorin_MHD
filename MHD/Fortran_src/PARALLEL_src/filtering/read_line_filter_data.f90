!
!     module read_line_filter_data
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine read_line_filter_data_a(file_code)
!      subroutine read_line_filter_data_b(file_code)
!
      module read_line_filter_data
!
      use m_precision
!
      use m_parallel_var_dof
      use m_geometry_parameter
      use m_l_filtering_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_line_filter_data_a(file_code)
!
      use skip_comment_f
!
      integer (kind = kint), intent(in) :: file_code
!
      integer (kind = kint) :: nd, inod, i
!
      character(len=255) :: character_4_read
      integer (kind = kint) :: itmp
      character (len = kchara) :: tmpchara
!
!
      call skip_comment(character_4_read,file_code)
      read(character_4_read,*) ndepth_l, num_filter_l
!
      do nd = 1, 3
        call skip_comment(character_4_read,file_code)
        read(character_4_read,*) itmp,                                  &
     &           nmax_l_filter(nd), nmin_l_filter(nd)
      end do
!
      call skip_comment(character_4_read,file_code)
      read(character_4_read,*) (num_l_filter(nd), nd=1,3)
!
!      write(*,*) 'allocate_l_filtering_data', numnod
!
      call allocate_l_filtering_data(numnod)
!
      call skip_comment(character_4_read,file_code)
      read(character_4_read,*) (inod_l_filter(1,nd), nd=1,3),           &
     &            (istack_l_filter(1,nd), nd=1,3)
      do inod = 2, numnod
        read(file_code,*) (inod_l_filter(inod,nd), nd=1,3),             &
     &            (istack_l_filter(inod,nd), nd=1,3)
      end do
!
      do nd = 1, 3
        read(file_code,*) tmpchara
        read(file_code,*)                                               &
     &            ( item_l_filter(i,nd), i=1,num_l_filter(nd) )
      end do
!
      do nd = 1, 3
        read(file_code,*) tmpchara
        read(file_code,*)                                               &
     &            ( coef_l_filter(i,nd), i=1,num_l_filter(nd) )
      end do
!
!
      end subroutine read_line_filter_data_a
!
!-----------------------------------------------------------------------
!
      subroutine read_line_filter_data_b(file_code)
!
!
      integer (kind = kint), intent(in) :: file_code
      integer (kind = kint) :: nd, inod, i
!
!
       read(file_code) ndepth_l, num_filter_l
!
       read(file_code) (nmax_l_filter(nd),nd=1, 3)
       read(file_code) (nmin_l_filter(nd),nd=1, 3)
!
       read(file_code) (num_l_filter(nd), nd=1,3)
!
       call allocate_l_filtering_data(numnod)
!
       read(file_code)                                                  &
     &       ((inod_l_filter(inod,nd),inod=1,numnod), nd=1,3)
       read(file_code)                                                  &
     &       ((istack_l_filter(inod,nd),inod=1,numnod), nd=1,3)
!
       read(file_code)                                                  &
     &       ((item_l_filter(i,nd),i=1,ntot_l_filter), nd=1,3)
!
       read(file_code) ((coef_l_filter(i,nd),i=1,ntot_l_filter),nd=1,3)
!
!
      end subroutine read_line_filter_data_b
!
!-----------------------------------------------------------------------
!
      end module read_line_filter_data
