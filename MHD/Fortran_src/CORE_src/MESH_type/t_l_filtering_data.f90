!
!      module t_l_filtering_data
!
!      Written by H. Matsui
!
!!      subroutine alloc_l_filtering_data(numnod, fil_l)
!!      subroutine alloc_l_filtering_smp(fil_l, fil_l_smp)
!!      subroutine dealloc_l_filtering_data(fil_l)
!!
!!      subroutine read_line_filter_data_a(file_code, numnod, fil_l)
!!      subroutine write_line_filter_data_a(file_code, fil_l)
!!
!!      subroutine check_istack_l_filter(id_rank, fil_l)
!!      subroutine check_istack_l_filter_smp                            &
!!     &         (id_rank, inod_smp_stack, fil_l_smp)
!
      module t_l_filtering_data
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      type line_filtering_type
        integer (kind = kint) :: ndepth_lf
        integer (kind = kint) :: num_filter_l
!
        integer (kind = kint) :: nnod_lf
        integer (kind = kint) :: ntot_lf
        integer (kind = kint) :: num_lf(3)
!
        integer (kind = kint) :: nmax_lf(3)
        integer (kind = kint) :: nmin_lf(3)
!
        integer (kind = kint) :: nsize_smp
!
!>        target local node ID for filtering
        integer (kind = kint), allocatable :: inod_lf(:,:)
!>        stack data for filtering
        integer (kind = kint), allocatable :: istack_lf(:,:)
!>        node information for filtering
        integer (kind = kint), allocatable :: item_lf(:,:)
!
!>        coefficients for filtering
        real (kind = kreal), allocatable :: coef_l(:,:)
      end type line_filtering_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_l_filtering_data(nnod, ndepth, nfilter, fil_l)
!
      integer(kind = kint), intent(in) :: nnod, ndepth, nfilter
      type(line_filtering_type), intent(inout) :: fil_l
!
!
      fil_l%ndepth_lf =    ndepth
      fil_l%num_filter_l = nfilter
      fil_l%nnod_lf = nnod
      fil_l%ntot_lf = max(fil_l%num_lf(1), fil_l%num_lf(2))
      fil_l%ntot_lf = max(fil_l%ntot_lf,   fil_l%num_lf(3))
!
      allocate( fil_l%inod_lf(fil_l%nnod_lf,3) )
      allocate( fil_l%istack_lf(0:fil_l%nnod_lf,3) )
      allocate( fil_l%item_lf(fil_l%ntot_lf,3) )
!
      allocate( fil_l%coef_l(fil_l%ntot_lf,3) )
!
      fil_l%inod_lf = 0
      fil_l%istack_lf = 0
      fil_l%item_lf = 0
!
      fil_l%coef_l = 0.0d0
!
      end subroutine alloc_l_filtering_data
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_l_filtering_smp(fil_l, fil_l_smp)
!
!
      type(line_filtering_type), intent(in) :: fil_l
      type(line_filtering_type), intent(inout) :: fil_l_smp
!
!
      fil_l_smp%ndepth_lf =    fil_l%ndepth_lf
      fil_l_smp%num_filter_l = fil_l%num_filter_l
      fil_l_smp%nnod_lf =      fil_l%nnod_lf
      fil_l_smp%ntot_lf =      fil_l%ntot_lf
!
      fil_l_smp%num_lf(1:3) =  fil_l%num_lf(1:3)
      fil_l_smp%nmax_lf(1:3) = fil_l%nmax_lf(1:3)
      fil_l_smp%nmin_lf(1:3) = fil_l%nmin_lf(1:3)
!
      allocate( fil_l_smp%inod_lf(fil_l_smp%nnod_lf,3) )

      allocate( fil_l_smp%istack_lf(0:fil_l_smp%nsize_smp*np_smp,3))
      allocate( fil_l_smp%item_lf(fil_l_smp%ntot_lf,3) )
!
      allocate( fil_l_smp%coef_l(fil_l_smp%ntot_lf,3) )
!
      fil_l_smp%inod_lf = 0
      fil_l_smp%istack_lf = 0
      fil_l_smp%item_lf = 0
!
      fil_l_smp%coef_l = 0.0d0
!
      end subroutine alloc_l_filtering_smp
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_l_filtering_data(fil_l)
!
      type(line_filtering_type), intent(inout) :: fil_l
!
      deallocate( fil_l%inod_lf )
      deallocate( fil_l%istack_lf )
      deallocate( fil_l%item_lf   )
      deallocate( fil_l%coef_l   )
!
      end subroutine dealloc_l_filtering_data
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_line_filter_data_a(file_code, numnod, fil_l)
!
      use skip_comment_f
!
      integer (kind = kint), intent(in) :: file_code, numnod
      type(line_filtering_type), intent(inout) :: fil_l
!
      integer (kind = kint) :: nd, inod
!
      character(len=255) :: character_4_read
      integer (kind = kint) :: ndepth, nfilter, itmp
      character (len = kchara) :: tmpchara
!
!
      call skip_comment(character_4_read,file_code)
      read(character_4_read,*) ndepth, nfilter
!
      do nd = 1, 3
        call skip_comment(character_4_read,file_code)
        read(character_4_read,*) itmp,                                  &
     &           fil_l%nmax_lf(nd), fil_l%nmin_lf(nd)
      end do
!
      call skip_comment(character_4_read,file_code)
      read(character_4_read,*) (fil_l%num_lf(nd), nd=1,3)
!
!
      call alloc_l_filtering_data(numnod, ndepth, nfilter, fil_l)
!
      call skip_comment(character_4_read,file_code)
      read(character_4_read,*) fil_l%inod_lf(1,1:3),                    &
     &                          fil_l%istack_lf(1,1:3)
      do inod = 2, fil_l%nnod_lf
        read(file_code,*) fil_l%inod_lf(inod,1:3),                      &
     &                    fil_l%istack_lf(inod,1:3)
      end do
!
      do nd = 1, 3
        read(file_code,*) tmpchara
        read(file_code,*) fil_l%item_lf(1:fil_l%num_lf(nd),nd)
      end do
!
      do nd = 1, 3
        read(file_code,*) tmpchara
        read(file_code,*) fil_l%coef_l(1:fil_l%num_lf(nd),nd)
      end do
!
      end subroutine read_line_filter_data_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_line_filter_data_a(file_code, fil_l)
!
      type(line_filtering_type), intent(in) :: fil_l
!
      integer (kind = kint) :: file_code
      integer (kind = kint) :: inod, nd
!
!
       write(file_code,'(a)')                                           &
     &        '! num_depth, max. number of node for filtering cube: '
       write(file_code,'(2i16)') fil_l%ndepth_lf, fil_l%num_filter_l
       write(file_code,'(a)')                                           &
     &     '! num_depth, max. and min. number of node for filtering: '
       do nd = 1, 3
         write(file_code,'(3i16)') nd, fil_l%nmax_lf(nd),               &
     &                                 fil_l%nmin_lf(nd)
       end do
       write(file_code,'(a)') '! total number of filtering data '
       write(file_code,'(3i16)') fil_l%num_lf(1:3)
!
!
       write(file_code,'(a)') '! orderind ID for filtering'
       write(file_code,'(a)')                                           &
     &     '! and stack for filtering for each direction'
       do inod = 1, fil_l%nnod_lf
          write(file_code,'(6i16)')  fil_l%inod_lf(inod,1:3),           &
      &                              fil_l%istack_lf(inod,1:3)
       enddo
!
          write(file_code,'(a)') '! node ID for x-filtering'
          write(file_code,'(8i16)') fil_l%item_lf(1:fil_l%num_lf(1),1)
          write(file_code,'(a)') '! node ID for y-filtering'
          write(file_code,'(8i16)') fil_l%item_lf(1:fil_l%num_lf(2),2)
          write(file_code,'(a)') '! node ID for z-filtering'
          write(file_code,'(8i16)') fil_l%item_lf(1:fil_l%num_lf(3),3)
!
          write(file_code,'(a)')                                        &
     &          '! filter coefficients for x'
          write(file_code,'(1p4E25.15e3)')                              &
     &           fil_l%coef_l(1:fil_l%num_lf(1),1)
          write(file_code,'(a)')                                        &
     &          '! filter coefficients for y'
          write(file_code,'(1p4E25.15e3)')                              &
     &           fil_l%coef_l(1:fil_l%num_lf(2),2)
          write(file_code,'(a)')                                        &
     &          '! filter coefficients for z'
          write(file_code,'(1p4E25.15e3)')                              &
     &           fil_l%coef_l(1:fil_l%num_lf(3),3)
!
!
      end subroutine write_line_filter_data_a
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_istack_l_filter(id_rank, fil_l)
!
      integer (kind = kint), intent(in) :: id_rank
      type(line_filtering_type), intent(in) :: fil_l
!
      integer (kind = kint) :: inod, nd
!
      write(50+id_rank,*) 'nd, inod, fil_l%istack_lf(inod, nd)'
      do inod = 1, fil_l%nnod_lf
       do nd = 1, 3
         write(50+id_rank,*) nd, inod, fil_l%istack_lf(inod, nd)
       end do
      end do
!
      end subroutine check_istack_l_filter
!
!  ---------------------------------------------------------------------
!
      subroutine check_istack_l_filter_smp                              &
     &         (id_rank, inod_smp_stack, fil_l_smp)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      type(line_filtering_type), intent(in) :: fil_l_smp
!
      integer(kind = kint) :: ist, ied, nd, ip, isum, ii
!
      write(50+id_rank,*) 'nd, ip, isum, fil_l_smp%istack_lf(i,ip,nd)'
      do ip = 1, np_smp
       ist = inod_smp_stack(ip-1) + 1
       ied = inod_smp_stack(ip)
       do nd = 1, 3
        do isum = 1, fil_l_smp%nmax_lf(nd)
          ii = (ip-1)*fil_l_smp%nsize_smp + isum
          write(50+id_rank,*) ip, nd, isum, fil_l_smp%istack_lf(ii,nd)
        end do
       end do
      end do
!
!
      end subroutine check_istack_l_filter_smp
!
!  ---------------------------------------------------------------------
!
      end module t_l_filtering_data
