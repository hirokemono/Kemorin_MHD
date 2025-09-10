!>@file   t_local_fline.f90
!!@brief  module t_local_fline
!!
!!@author H.Matsui
!!@date      Programmed in June, 2024
!
!>@brief  local field line and tracer data structure
!!
!!@verbatim
!!      subroutine reset_fline_start(fline_lc)
!!      subroutine add_fline_start(xx4_add, xi4_add, v4_add,            &
!!     &                           ntot_comp, col_add, fline_lc)
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: xx4_add(4)
!!        real(kind = kreal), intent(in) :: xi4_add(4)
!!        real(kind = kreal), intent(in) :: v4_add(4)
!!        real(kind = kreal), intent(in) :: col_add(ntot_comp)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!      subroutine alloc_local_fline(viz_fields, fline_lc)
!!        type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!      subroutine dealloc_local_fline(fline_lc)
!!      subroutine add_fline_list(iglobal_add, xx4_add, xi4_add, v4_add,&
!!     &                          ntot_comp, col_add, fline_lc)
!!        integer(kind = kint_gl), intent(in) :: iglobal_add
!!        real(kind = kreal), intent(in) :: xx4_add(4)
!!        real(kind = kreal), intent(in) :: xi4_add(4)
!!        real(kind = kreal), intent(in) :: v4_add(4)
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: col_add(ntot_comp)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      subroutine raise_local_fline_connect(fline_lc)
!!      subroutine raise_local_fline_data(fline_lc)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      subroutine check_local_fline(id_file, fline_lc)
!!        type(local_fieldline), intent(in) :: fline_lc
!!@endverbatim
!
      module t_local_fline
!
      use m_precision
      use m_constants
!
      implicit  none
!
      type local_fieldline
        integer(kind = kint) :: nele_line_buf
        integer(kind = kint) :: nele_line_l
        integer(kind = kint), allocatable :: iedge_line_l(:,:)
!
        integer(kind = kint) :: nnod_line_buf
        integer(kind = kint) :: nnod_line_l
        integer(kind = kint) :: ntot_comp_l
        integer(kind = kint_gl), allocatable :: iglobal_fline(:)
        real(kind = kreal), allocatable ::   xx_line_l(:,:)
        real(kind = kreal), allocatable ::   xi_line_l(:,:)
        real(kind = kreal), allocatable ::   v_line_l(:,:)
        real(kind = kreal), allocatable ::   col_line_l(:,:)
      end type local_fieldline
!
      private :: alloc_local_fline_data
      private :: dealloc_local_fline_conn, dealloc_local_fline_data
      private :: copy_local_fline_connect, copy_local_fline_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reset_fline_start(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      fline_lc%nnod_line_l = 0
      fline_lc%nele_line_l = 0
!
      end subroutine reset_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine add_fline_start(xx4_add, xi4_add, v4_add,              &
     &                           ntot_comp, col_add, fline_lc)
!
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: xx4_add(4)
      real(kind = kreal), intent(in) :: xi4_add(4)
      real(kind = kreal), intent(in) :: v4_add(4)
      real(kind = kreal), intent(in) :: col_add(ntot_comp)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      fline_lc%xx_line_l(1:3,fline_lc%nnod_line_l) = xx4_add(1:3)
      fline_lc%xi_line_l(1:3,fline_lc%nnod_line_l) = xi4_add(1:3)
      fline_lc%v_line_l(1:3,fline_lc%nnod_line_l) =  v4_add(1:3)
      fline_lc%col_line_l(1:ntot_comp,fline_lc%nnod_line_l)             &
     &       = col_add(1:ntot_comp)
!
      end subroutine add_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine add_fline_list(iglobal_add, xx4_add, xi4_add, v4_add,  &
     &                          ntot_comp, col_add, fline_lc)
!
      integer(kind = kint_gl), intent(in) :: iglobal_add
      real(kind = kreal), intent(in) :: xx4_add(4)
      real(kind = kreal), intent(in) :: xi4_add(4)
      real(kind = kreal), intent(in) :: v4_add(4)
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: col_add(ntot_comp)
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      if(fline_lc%nele_line_l .ge. fline_lc%nele_line_buf) then
         call raise_local_fline_connect(fline_lc)
      end if
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
!
      fline_lc%nele_line_l = fline_lc%nele_line_l + 1
      fline_lc%nnod_line_l = fline_lc%nnod_line_l + 1
!
      fline_lc%iedge_line_l(1,fline_lc%nele_line_l)                     &
     &      = fline_lc%nnod_line_l - 1
      fline_lc%iedge_line_l(2,fline_lc%nele_line_l)                     &
     &      = fline_lc%nnod_line_l
!
      fline_lc%iglobal_fline(fline_lc%nnod_line_l) = iglobal_add
      fline_lc%xx_line_l(1:3,fline_lc%nnod_line_l) = xx4_add(1:3)
      fline_lc%xi_line_l(1:3,fline_lc%nnod_line_l) = xi4_add(1:3)
      fline_lc%v_line_l(1:3,fline_lc%nnod_line_l) =  v4_add(1:3)
      fline_lc%col_line_l(1:ntot_comp,fline_lc%nnod_line_l)             &
     &      = col_add(1:ntot_comp)
!
      end subroutine add_fline_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_fline(viz_fields, fline_lc)
!
      use t_ctl_params_viz_fields
!
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      call reset_fline_start(fline_lc)
!
      fline_lc%ntot_comp_l = viz_fields%ntot_color_comp
      call alloc_local_fline_conn(ione, fline_lc)
      call alloc_local_fline_data(itwo, fline_lc)
!
      end subroutine alloc_local_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_fline(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      call dealloc_local_fline_conn(fline_lc)
      call dealloc_local_fline_data(fline_lc)
!
      end subroutine dealloc_local_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine raise_local_fline_connect(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      type(local_fieldline) :: fline_tmp
!
!
      call alloc_local_fline_conn(fline_lc%nele_line_l, fline_tmp)
      call copy_local_fline_connect(fline_lc%nele_line_l, fline_lc,     &
     &                              fline_tmp)
!
      call dealloc_local_fline_conn(fline_lc)
      call alloc_local_fline_conn(itwo*fline_lc%nele_line_l, fline_lc)
!
      call copy_local_fline_connect(fline_tmp%nele_line_l, fline_tmp,   &
     &                              fline_lc)
      call dealloc_local_fline_conn(fline_tmp)
!
      end subroutine raise_local_fline_connect
!
!  ---------------------------------------------------------------------
!
      subroutine raise_local_fline_data(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      type(local_fieldline) :: fline_tmp
!
!
      fline_tmp%ntot_comp_l = fline_lc%ntot_comp_l
      call alloc_local_fline_data(fline_lc%nnod_line_l, fline_tmp)
      call copy_local_fline_data(fline_lc%nnod_line_l, fline_lc,        &
     &                           fline_tmp)
!
      call dealloc_local_fline_data(fline_lc)
      call alloc_local_fline_data(itwo*fline_lc%nnod_line_l, fline_lc)
!
      call copy_local_fline_data(fline_tmp%nnod_line_l, fline_tmp,      &
     &                           fline_lc)
      call dealloc_local_fline_data(fline_tmp)
!
      end subroutine raise_local_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_local_fline_connect(nele_line_l, fline_lc,        &
     &                                    fline_new)
!
      integer(kind = kint), intent(in) :: nele_line_l
      type(local_fieldline), intent(in) :: fline_lc
      type(local_fieldline), intent(inout) :: fline_new
!
      integer(kind = kint) :: i
!
!
!$omp parallel do
      do i = 1, nele_line_l
        fline_new%iedge_line_l(1,i) = fline_lc%iedge_line_l(1,i)
        fline_new%iedge_line_l(2,i) = fline_lc%iedge_line_l(2,i)
      end do
!$omp end parallel do
!
      end subroutine copy_local_fline_connect
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_fline_data(nnod_line_l, fline_lc,           &
     &                                 fline_new)
!
      integer(kind = kint), intent(in) :: nnod_line_l
      type(local_fieldline), intent(in) :: fline_lc
      type(local_fieldline), intent(inout) :: fline_new
!
      integer(kind = kint) :: i
!
!
!$omp parallel do
      do i = 1, nnod_line_l
        fline_new%iglobal_fline(i) = fline_lc%iglobal_fline(i)
        fline_new%xx_line_l(1,i) =   fline_lc%xx_line_l(1,i)
        fline_new%xx_line_l(2,i) =   fline_lc%xx_line_l(2,i)
        fline_new%xx_line_l(3,i) =   fline_lc%xx_line_l(3,i)
        fline_new%xi_line_l(1,i) =   fline_lc%xi_line_l(1,i)
        fline_new%xi_line_l(2,i) =   fline_lc%xi_line_l(2,i)
        fline_new%xi_line_l(3,i) =   fline_lc%xi_line_l(3,i)
        fline_new%v_line_l(1,i) =    fline_lc%v_line_l(1,i)
        fline_new%v_line_l(2,i) =    fline_lc%v_line_l(2,i)
        fline_new%v_line_l(3,i) =    fline_lc%v_line_l(3,i)
        fline_new%col_line_l(1:fline_lc%ntot_comp_l,i)                  &
     &           =  fline_lc%col_line_l(1:fline_lc%ntot_comp_l,i)
      end do
!$omp end parallel do
!
      end subroutine copy_local_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_fline_conn(nele_buf, fline_lc)
!
      integer(kind = kint), intent(in) :: nele_buf
      type(local_fieldline), intent(inout) :: fline_lc
!
      fline_lc%nele_line_buf = nele_buf
      allocate(fline_lc%iedge_line_l(2,fline_lc%nele_line_buf))
      if(fline_lc%nele_line_buf .gt. 0) fline_lc%iedge_line_l =  0
!
      end subroutine alloc_local_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_fline_data(nnod_buf, fline_lc)
!
      integer(kind = kint), intent(in) :: nnod_buf
      type(local_fieldline), intent(inout) :: fline_lc
!
      fline_lc%nnod_line_buf = nnod_buf
      allocate(fline_lc%iglobal_fline(fline_lc%nnod_line_buf))
      allocate(fline_lc%xx_line_l(3,fline_lc%nnod_line_buf))
      allocate(fline_lc%xi_line_l(3,fline_lc%nnod_line_buf))
      allocate(fline_lc%v_line_l(3,fline_lc%nnod_line_buf))
      allocate(fline_lc%col_line_l(fline_lc%ntot_comp_l,                &
     &                             fline_lc%nnod_line_buf))
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%iglobal_fline = 0
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%xx_line_l =  0.0d0
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%xi_line_l =  0.0d0
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%v_line_l =   0.0d0
      if(fline_lc%nnod_line_buf .gt. 0) fline_lc%col_line_l = 0.0d0
!
      end subroutine alloc_local_fline_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_fline_conn(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      deallocate(fline_lc%iedge_line_l)
!
      end subroutine dealloc_local_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_fline_data(fline_lc)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
!
      deallocate(fline_lc%xx_line_l, fline_lc%xi_line_l)
      deallocate(fline_lc%v_line_l)
      deallocate(fline_lc%iglobal_fline, fline_lc%col_line_l)
!
      end subroutine dealloc_local_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_local_fline(id_file, fline_lc)
!
      integer(kind = kint), intent(in) :: id_file
      type(local_fieldline), intent(in) :: fline_lc
      integer(kind = kint) :: i, nd
!
!
      write(id_file,*) 'xx_line_l', fline_lc%nnod_line_l
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(i16,1p3e16.7)') i, fline_lc%xx_line_l(1:3,i)
      end do
!
      write(id_file,*) 'v_line_l', fline_lc%nnod_line_l
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(i16,1p3e16.7)') i, fline_lc%v_line_l(1:3,i)
      end do
!
      write(id_file,*) 'iedge_line_l', fline_lc%nele_line_l
      do i = 1, fline_lc%nele_line_l
        write(id_file,'(2i16,a7,2i16)') i, ione, '  line ',             &
     &                                 fline_lc%iedge_line_l(1:2,i)
      end do
!
      write(id_file,'(2i4)') ione, ione
      write(id_file,'(a)') 'color col_line_l,'
      do i = 1, fline_lc%nnod_line_l
        write(id_file,'(i16)', ADVANCE='NO') i
        do nd = 1, fline_lc%ntot_comp_l
           write(id_file,'(i16,1pe16.7)', ADVANCE='NO')                 &
     &       fline_lc%iglobal_fline(i), fline_lc%col_line_l(nd,i)
        end do
        write(id_file,*)
      end do
!
      close(id_file)
!
      end subroutine check_local_fline
!
!  ---------------------------------------------------------------------
!
      subroutine check_tracer_restarts                                  &
     &         (i_fln, mesh, nod_fld, fln_prm, fline_lc)
!
      use t_mesh_data
      use t_phys_data
      use t_control_params_4_fline
      use calypso_mpi
      use field_at_each_seed_point
!
      use t_find_interpolate_in_ele
!
      integer(kind = kint), intent(in) :: i_fln
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(local_fieldline), intent(inout) :: fline_lc
!
      integer(kind = kint) :: ip, i
      integer(kind = kint) :: ierr_inter, iflag
      real(kind = kreal) :: v_fline_start(4)
      type(cal_interpolate_coefs_work) :: itp_ele_work_g
!
      integer(kind = kint), parameter :: maxitr = 20
      real(kind = kreal), parameter ::   eps_iter = 1.0d-9
      integer(kind = kint), parameter :: iflag_nomessage = 0
      real(kind = kreal), parameter ::   error_level = 1.0d-9
!
!
      write(*,*) 'calypso_mpi_barrier', my_rank, fline_lc%nnod_line_l
      call calypso_mpi_barrier()

          call alloc_work_4_interpolate(mesh%ele%nnod_4_ele,            &
     &                                  itp_ele_work_g)
          do ip = 1, nprocs
            call calypso_mpi_barrier()
            if(my_rank .eq. (ip-1)) then
              write(*,*) i_fln, my_rank, 'restart: ',                   &
     &                  fline_lc%nnod_line_l
              do i = 1, fline_lc%nnod_line_l
                call find_interpolate_in_ele                            &
     &             (fline_lc%xx_line_l(1,i), maxitr, eps_iter,          &
     &              my_rank, iflag_nomessage, error_level,              &
     &              mesh%node, mesh%ele,                                &
     &              fline_lc%iedge_line_l(1,i), itp_ele_work_g,         &
     &              fline_lc%xi_line_l(1,i), ierr_inter)
                iflag = surface_mode_in_each_ele(error_level,           &
     &                                         fline_lc%xi_line_l(1,i))
                call cal_each_seed_velocity_in_ele                      &
     &             (mesh%ele, nod_fld%n_point,                          &
     &              nod_fld%d_fld(1,fln_prm%iphys_4_fline),             &
     &              fline_lc%iedge_line_l(1,i),                         &
     &              fline_lc%xi_line_l(1:3,i), v_fline_start)
                write(*,*) i, fline_lc%iedge_line_l(1,i),        &
     &                        fline_lc%xx_line_l(1:3,i),         &
     &                        fline_lc%iedge_line_l(2,i), iflag, &
     &                        fline_lc%xi_line_l(1:3,i), ierr_inter,    &
     &                 v_fline_start(1), fline_lc%v_line_l(1,i), &
     &                 v_fline_start(2), fline_lc%v_line_l(2,i), &
     &                 v_fline_start(3), fline_lc%v_line_l(3,i)
              end do
            end if
          end do
          call calypso_mpi_barrier()
          call dealloc_work_4_interpolate(itp_ele_work_g)
!
      end subroutine check_tracer_restarts
!
!  ---------------------------------------------------------------------
!
      end module t_local_fline
