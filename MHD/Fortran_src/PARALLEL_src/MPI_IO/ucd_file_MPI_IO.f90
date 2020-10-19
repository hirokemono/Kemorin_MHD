!>@file  ucd_file_MPI_IO.f90
!!       module ucd_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine write_ucd_data_mpi_b(IO_param_l,                     &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod, istack_merged_intnod)
!!      subroutine write_ucd_mesh_data_mpi_b                            &
!!     &         (IO_param_l, nnod, nele, nnod_ele, xx, ie,             &
!!     &          istack_merged_intnod, istack_merged_ele)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!!
!!      subroutine write_ucd_data_mpi(id_vtk, ioff_gl,                  &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod, istack_merged_intnod)
!!      subroutine write_ucd_node_mpi(id_vtk, ioff_gl, nnod,            &
!!     &          ntot_comp, xx, istack_merged_intnod, istack_merged_ele)
!!      subroutine write_ucd_connect_mpi(id_vtk, ioff_gl,               &
!!     &          nele, nnod_ele, ie, istack_merged_ele)
!!@endverbatim
!
      module ucd_file_MPI_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use vtk_data_to_buffer
      use ucd_data_to_buffer
      use t_calypso_mpi_IO_param
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_data_w_time_mpi_b(IO_param_l,                &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged_intnod)
!
      use t_calypso_mpi_IO_param
      use MPI_binary_head_IO
      use MPI_binary_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind=kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint) :: nd
      integer(kind = kint_gl) :: n_internal(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      n_internal(1) = istack_merged_intnod(my_rank+1)                   &
     &               - istack_merged_intnod(my_rank)
!
      call mpi_write_process_id_b(IO_param_l)
!
      call mpi_write_one_inthead_b(IO_param_l, i_time_step_IO)
      call mpi_write_one_realhead_b(IO_param_l, time_IO)
      call mpi_write_one_realhead_b(IO_param_l, delta_t_IO)
!
      call set_istack_4_fixed_num(ione, IO_param_l)
      call mpi_write_int8_vector_b(IO_param_l, ione64, n_internal(1))
!
      call mpi_write_one_inthead_b(IO_param_l, num_field)
      call mpi_write_mul_inthead_b(IO_param_l, num_field, ncomp_field)
      call mpi_write_mul_charahead_b(IO_param_l, num_field, field_name)
!
      call istack64_4_parallel_data(n_internal(1), IO_param_l)
      do nd = 1, ntot_comp
        call mpi_write_1d_vector_b                                      &
     &     (IO_param_l, n_internal(1), d_nod(1,nd))
      end do
!
      end subroutine write_ucd_data_w_time_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_data_mpi_b(IO_param_l,                       &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged_intnod)
!
      use t_calypso_mpi_IO_param
      use MPI_binary_head_IO
      use MPI_binary_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint) :: nd
      integer(kind = kint_gl) :: n_internal(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      n_internal(1) = istack_merged_intnod(my_rank+1)                   &
     &               - istack_merged_intnod(my_rank)
!
      call mpi_write_process_id_b(IO_param_l)
!
      call set_istack_4_fixed_num(ione, IO_param_l)
      call mpi_write_int8_vector_b(IO_param_l, ione64, n_internal(1))
!
      call mpi_write_one_inthead_b(IO_param_l, num_field)
      call mpi_write_mul_inthead_b(IO_param_l, num_field, ncomp_field)
      call mpi_write_mul_charahead_b(IO_param_l, num_field, field_name)
!
      call istack64_4_parallel_data(n_internal(1), IO_param_l)
      do nd = 1, ntot_comp
        call mpi_write_1d_vector_b                                      &
     &     (IO_param_l, n_internal(1), d_nod(1,nd))
      end do
!
      end subroutine write_ucd_data_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_mesh_data_mpi_b                              &
     &         (IO_param_l, nnod, nele, nnod_ele, xx, ie,               &
     &          istack_merged_intnod)
!
      use m_phys_constants
      use MPI_binary_data_IO
      use MPI_binary_head_IO
      use set_nnod_4_ele_by_type
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param_l
!
      integer(kind = kint) :: nd
      integer(kind = kint_gl) :: n_internal(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint_gl) :: num64(1)
!
!
      n_internal(1) = istack_merged_intnod(my_rank+1)                   &
     &               - istack_merged_intnod(my_rank)
!
      call mpi_write_process_id_b(IO_param_l)
!
      call set_istack_4_fixed_num(ione, IO_param_l)
      call mpi_write_int8_vector_b(IO_param_l, ione64, n_internal(1))
!
      call istack64_4_parallel_data(n_internal(1), IO_param_l)
      call mpi_write_1d_vector_b(IO_param_l, n_internal(1), xx(1,1))
      call mpi_write_1d_vector_b(IO_param_l, n_internal(1), xx(1,2))
      call mpi_write_1d_vector_b(IO_param_l, n_internal(1), xx(1,3))
!
      call mpi_write_one_inthead_b(IO_param_l, nnod_ele)
      call mpi_write_one_inthead_b                                      &
     &   (IO_param_l, linear_eletype_from_num(nnod_ele))
!
      num64(1) = nele
      call set_istack_4_fixed_num(ione, IO_param_l)
      call mpi_write_int8_vector_b(IO_param_l, ione64, num64(1))
!
      call istack64_4_parallel_data(num64(1), IO_param_l)
      do nd = 1, nnod_ele
        call mpi_write_int8_vector_b(IO_param_l, num64(1), ie(1,nd))
      end do
!
      end subroutine write_ucd_mesh_data_mpi_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_data_mpi(id_vtk, ioff_gl,                    &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged_intnod)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint) :: j
      integer(kind = kint_gl) :: inod, num, nt_nod, inod_gl
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      real(kind = kreal)  :: dat_1(ntot_comp)
!
      integer :: ilen_n
      character(len=16+ntot_comp*23+1), target :: textbuf_n(nnod)
!
      ilen_n = int(16+ntot_comp*23+1)
      nt_nod = istack_merged_intnod(nprocs)
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      call calypso_mpi_seek_write_head_c(id_vtk, ioff_gl,               &
     &   ucd_num_comps(num_field, ncomp_field))
!
      do j = 1, num_field
        call calypso_mpi_seek_write_head_c(id_vtk, ioff_gl,             &
     &      ucd_field_name(field_name(j)))
      end do
!
      ioffset = ioff_gl + ilen_n * istack_merged_intnod(my_rank)
      ioff_gl = ioff_gl + ilen_n * nt_nod
!
      if(num .le. 0) return
!
      do inod = 1, num
        inod_gl =    inod + istack_merged_intnod(my_rank)
        dat_1(1:ntot_comp) = d_nod(inod,1:ntot_comp)
        textbuf_n(inod) = ucd_each_field(inod_gl, ntot_comp, dat_1)
      end do
      call mpi_write_mul_chara_b                                        &
     &   (id_vtk, ioffset, ilen_n, num, textbuf_n(1))
!
      end subroutine write_ucd_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_node_mpi(id_vtk, ioff_gl, nnod,              &
     &          ntot_comp, xx, istack_merged_intnod, istack_merged_ele)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_ele(0:nprocs)
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: inod_gl
      integer(kind = kint_gl) :: inod, nt_nod, nt_ele, num
      real(kind = kreal)  :: dat_1(n_vector)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer, parameter :: ilen_n = 16+n_vector*23+1
      character(len=ilen_n), target  :: textbuf_n(nnod)
!
!
      nt_nod = istack_merged_intnod(nprocs)
      nt_ele = istack_merged_ele(nprocs)
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      call calypso_mpi_seek_write_head_c(id_vtk, ioff_gl,               &
     &   ucd_connect_head(nt_nod, nt_ele, ntot_comp))

      ioffset = ioff_gl + ilen_n * istack_merged_intnod(my_rank)
      ioff_gl = ioff_gl + ilen_n * nt_nod
!
      if(num .le. 0) return
!
      do inod = 1, num
        inod_gl =    inod + istack_merged_intnod(my_rank)
        dat_1(1:n_vector) = xx(inod,1:n_vector)
        textbuf_n(inod) = ucd_each_field(inod_gl, n_vector, dat_1)
      end do
      call mpi_write_mul_chara_b                                        &
     &   (id_vtk, ioffset, ilen_n, num, textbuf_n)
!
      end subroutine write_ucd_node_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_connect_mpi(id_vtk, ioff_gl,                 &
     &          nele, nnod_ele, ie, istack_merged_ele)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_ele(0:nprocs)
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: ie0(nnod_ele), iele_gl
      integer(kind = kint_gl) :: iele, nt_ele
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer :: ilen_e
      character(len=16+3+6+16*nnod_ele+1), target :: textbuf_e(nele)
!
!
      nt_ele = istack_merged_ele(nprocs)
!
      ilen_e = int(16+3+6+16*nnod_ele+1)
      ioffset = ioff_gl + ilen_e * istack_merged_ele(my_rank)
      ioff_gl = ioff_gl + ilen_e * nt_ele
!
      if(nele .le. 0) return
!
      do iele = 1, nele
        iele_gl = iele + istack_merged_ele(my_rank)
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele)
        textbuf_e(iele) = ucd_each_connect(iele_gl, nnod_ele,ie0)
      end do
      call mpi_write_mul_chara_b                                        &
     &   (id_vtk, ioffset, ilen_e, nele, textbuf_e)
!
      end subroutine write_ucd_connect_mpi
!
! -----------------------------------------------------------------------
!
      end module ucd_file_MPI_IO
